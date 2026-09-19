;;; gemit.el --- Generate commit messages with Google Gemini  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "4.0.0"))
;; Keywords: tools, vc
;; URL: https://github.com/nqminhuit/gemit

;;; Commentary:

;; Generate conventional commit messages from staged diffs with Google
;; Gemini.  Self-contained: besides Magit, only Emacs built-ins are used
;; (`json-parse-buffer' and `json-serialize' are native since Emacs 27).
;;
;; Setup (Doom):
;;   (package! gemit :recipe (:host github :repo "nqminhuit/gemit"))
;;   (setopt gemit-model "gemini-flash-lite-latest")
;;   (with-eval-after-load 'magit (gemit-install))
;;
;; Setup (vanilla):
;;   (require 'gemit)
;;   (with-eval-after-load 'magit (gemit-install))
;;
;; The API key is looked up from `gemit-api-key', the
;; GEMINI_API_KEY_GEMIT environment variable, auth-source, then prompted
;; once per session.  After `gemit-install', generate from a git-commit
;; buffer, or with "g" in the magit-commit transient.

;;; Code:

(require 'url)
(require 'subr-x)

;; Magit is a hard dependency at runtime but not at compile time, so plain
;; `emacs -Q' (as in CI) can byte-compile this file without it installed.
(declare-function magit-commit-message-buffer "magit-commit" ())
(declare-function magit-git-output "magit-git" (&rest args))
(declare-function magit-commit-arguments "magit-commit" (&optional mode))
(declare-function magit-commit-create "magit-commit" (&optional args))
(declare-function transient-append-suffix "transient" (prefix loc suffix &optional keep-other))
(defvar git-commit-mode-map)
(defvar git-commit-summary-max-length)
(defvar url-http-end-of-headers)

(defgroup gemit nil
  "Generate commit messages with Google Gemini."
  :group 'tools
  :prefix "gemit-")

(defcustom gemit-commit-prompt
  "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes. Follow closely the Conventional Commits (https://www.conventionalcommits.org/en/v1.0.0/). The commit message should be structured as follows:

    <type>(<optional scope>): <description>

    [optional body]"
  "Prompt for commit generation; input is the staged diff."
  :type 'string
  :group 'gemit)

(defcustom gemit-model "gemini-flash-lite-latest"
  "Google Gemini model to use."
  :type 'string
  :group 'gemit)

(defcustom gemit-api-key nil
  "Google Gemini API key, or a file holding it.
When nil, the key is looked up from the GEMINI_API_KEY_GEMIT environment
variable, then auth-source, then prompted (cached for the session)."
  :type '(choice string (const nil))
  :group 'gemit)

(defvar gemit--api-key-cache nil
  "Session cache for the Gemini API key.")

(defun gemit--resolve-api-key (key-or-file)
  "Return KEY-OR-FILE itself, or the content of the file it names."
  (if (and (stringp key-or-file) (file-exists-p key-or-file))
      (string-trim (with-temp-buffer
                     (insert-file-contents key-or-file)
                     (buffer-string)))
    key-or-file))

(defun gemit--api-key ()
  "Return the Gemini API key: setting, env, auth-source, then prompt."
  (or gemit--api-key-cache
      (let ((key (or (gemit--resolve-api-key gemit-api-key)
                     (getenv "GEMINI_API_KEY_GEMIT")
                     (ignore-errors
                       (auth-source-pick-first-password
                        :host "generativelanguage.googleapis.com"))
                     (read-passwd "Gemini API key (paste with C-y): "))))
        (when (and (stringp key) (not (string-empty-p (string-trim key))))
          (setq gemit--api-key-cache (string-trim key))))))

;;; Gemini API call

(defun gemit--response-text (resp)
  "Return text from Gemini response RESP."
  (when-let* ((cands (plist-get resp :candidates))
              (parts (plist-get (plist-get (elt cands 0) :content) :parts)))
    (plist-get (elt parts 0) :text)))

(defun gemit--invalid-key-error-p (msg)
  "Non-nil if MSG means the API rejected the key itself.
Other failures (a restricted project, quota, model issues) leave a
correctly-typed key alone, so they never trigger a re-prompt."
  (and (stringp msg)
       (let ((case-fold-search t))
         (string-match-p "api[_ ]key[_ ]\\(not valid\\|expired\\|invalid\\)\\|api keys are not supported"
                         msg))))

(defun gemit--api-error (status-error)
  "Describe STATUS-ERROR, preferring the API's own error message."
  (or (ignore-errors
        (goto-char url-http-end-of-headers)
        (plist-get (plist-get (json-parse-buffer :object-type 'plist) :error)
                   :message))
      (format "Connection error: %s" status-error)))

(defun gemit--request-async (prompt system callback)
  "Send PROMPT and SYSTEM to Gemini API asynchronously.
Calls CALLBACK with (RESPONSE nil) on success or (nil ERROR-MSG) on failure."
  (let ((api-key (gemit--api-key)))
    (unless api-key
      (user-error "Google Gemini API key not set"))
    (let ((url-request-method "POST")
          (url-request-data
           (encode-coding-string
            (json-serialize
             (list :systemInstruction (list :parts (vector (list :text system)))
                   :contents (vector (list :parts (vector (list :text prompt))))))
            'utf-8))
          (url-mime-accept-string "application/json")
          (url-request-extra-headers '(("content-type" . "application/json"))))
      (url-retrieve
       (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
               gemit-model api-key)
       (lambda (status)
         (condition-case err
             (if-let* ((e (plist-get status :error)))
                 (let ((msg (gemit--api-error e)))
                   ;; A rejected key is forgotten, so the next attempt
                   ;; re-prompts instead of failing the whole session.
                   ;; Keys from the setting/env obviously resolve again,
                   ;; so only prompted keys effectively retry.
                   (when (gemit--invalid-key-error-p msg)
                     (setq gemit--api-key-cache nil))
                   (funcall callback nil msg))
               (goto-char url-http-end-of-headers)
               (let* ((resp (json-parse-buffer :object-type 'plist))
                      (text (gemit--response-text resp)))
                 (if text
                     (funcall callback text nil)
                   (funcall callback nil
                            (or (plist-get (plist-get resp :error) :message)
                                "No response content from Gemini API")))))
           (error (funcall callback nil (error-message-string err))))
         (kill-buffer (current-buffer)))
       nil t))))

;;; Commit message formatting

(defun gemit--format-commit-message (message)
  "Format commit message MESSAGE nicely."
  (with-temp-buffer
    (insert message)
    (text-mode)
    (setq fill-column git-commit-summary-max-length)
    (fill-region (point-min) (point-max))
    (buffer-string)))

;;; Commit message generation commands

(defun gemit--generate (on-success)
  "Generate a commit message from the staged diff.
Calls ON-SUCCESS with the formatted message, or shows the API error."
  (let ((diff (magit-git-output "diff" "--cached")))
    (when (string-empty-p (string-trim diff))
      (user-error "No staged changes"))
    (message "gemit: Generating...")
    (gemit--request-async
     diff gemit-commit-prompt
     (lambda (response error-msg)
       (if response
           (funcall on-success (gemit--format-commit-message response))
         (message "gemit: %s" (or error-msg "Unknown error")))))))

;;;###autoload
(defun gemit-generate-message ()
  "Generate a commit message when in the git commit buffer."
  (interactive)
  (unless (magit-commit-message-buffer)
    (user-error "No commit in progress"))
  (gemit--generate
   (lambda (message)
     (with-current-buffer (magit-commit-message-buffer)
       (save-excursion
         (goto-char (point-min))
         (insert message))
       (message "gemit: Commit message generated")))))

;;;###autoload
(defun gemit-commit-generate (&optional args)
  "Create a new commit with a generated commit message.
Uses ARGS from transient mode."
  (interactive (list (magit-commit-arguments)))
  (gemit--generate
   (lambda (message)
     (magit-commit-create (append args `("--message" ,message "--edit")))
     (message "gemit: Commit created"))))

;;;###autoload
(defun gemit-install ()
  "Bind \\[gemit-generate-message] and add the magit-commit transient suffix."
  (define-key git-commit-mode-map (kbd "M-g") 'gemit-generate-message)
  (transient-append-suffix 'magit-commit #'magit-commit-create
    '("g" "Generate commit" gemit-commit-generate)))

(provide 'gemit)
;;; gemit.el ends here
