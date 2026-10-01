;;; gemit.el --- Generate commit messages with a local LLM or Gemini  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit "4.0.0"))
;; Keywords: tools, vc
;; URL: https://github.com/nqminhuit/gemit

;;; Commentary:

;; Generate conventional commit messages from staged diffs, preferring a
;; local llama.cpp server and asking before sending a diff to Google Gemini.
;; Self-contained: besides Magit, only Emacs built-ins are used.
;;
;; Setup (Doom):
;;   (package! gemit :recipe (:host github :repo "nqminhuit/gemit"))
;;   (setopt gemit-backend 'auto)
;;   (with-eval-after-load 'magit (gemit-install))
;;
;; Setup (vanilla):
;;   (require 'gemit)
;;   (with-eval-after-load 'magit (gemit-install))
;;
;; Start a compatible llama.cpp server at `gemit-local-url' to use the
;; default local-first backend.  Gemini keys are only looked up after
;; consenting to cloud fallback, or when `gemit-backend' is `gemini'.
;; After `gemit-install', generate from a git-commit buffer, or with "g"
;; in the magit-commit transient.

;;; Code:

(require 'url)
(require 'subr-x)
(require 'cl-lib)

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
  "Generate commit messages with a local LLM or Google Gemini."
  :group 'tools
  :prefix "gemit-")

(defcustom gemit-backend 'auto
  "Backend for generating commit messages.
`auto' tries the local server, then asks before sending the diff to Gemini.
`local' uses only the local server; `gemini' sends directly to Google."
  :type '(choice (const :tag "Local first, ask before Gemini fallback" auto)
                 (const :tag "Local only" local)
                 (const :tag "Google Gemini only" gemini))
  :group 'gemit)

(defcustom gemit-local-url "http://127.0.0.1:8080"
  "Root URL of a compatible llama.cpp server."
  :type 'string
  :group 'gemit)

(defcustom gemit-local-model nil
  "Model alias for the local server, or nil to discover its single model."
  :type '(choice (const :tag "Discover the loaded model" nil) string)
  :group 'gemit)

(defcustom gemit-local-availability-timeout 2
  "Seconds to wait for each local health or model-discovery request."
  :type 'number
  :group 'gemit)

(defcustom gemit-local-generation-timeout 120
  "Seconds to wait for local commit-message generation."
  :type 'number
  :group 'gemit)

(defconst gemit--local-health-path "/health")
(defconst gemit--local-models-path "/v1/models")
(defconst gemit--local-completions-path "/v1/chat/completions")
(defconst gemit--json-content-type "application/json")
(defconst gemit--api-key-env-var "GEMINI_API_KEY_GEMIT")

(defcustom gemit-commit-prompt
  "Write a Conventional Commits message for the given diff.

Format:
<type>(<scope>): <description>

<body>

Rules:
- type: feat|fix|refactor|perf|test|docs|style|chore|build|ci
- description: imperative mood, lowercase, no period, ≤50 chars
- scope: jiraTicketID if available (from branch name or provided context); otherwise omit scope entirely, including the parentheses
- body: only if the diff needs context beyond the summary line; wrap at 72 chars; omit for trivial/small changes
- Base everything strictly on the diff. No speculation about intent.
- Output ONLY the commit message. No preamble, no explanation, no markdown formatting/backticks."
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

(defun gemit--api-key-for-setting (key-setting)
  "Return the Gemini key using KEY-SETTING before the environment and prompt."
  (or gemit--api-key-cache
      (let ((key (or (gemit--resolve-api-key key-setting)
                     (getenv gemit--api-key-env-var)
                     (ignore-errors
                       (auth-source-pick-first-password
                        :host "generativelanguage.googleapis.com"))
                     (read-passwd "Gemini API key (paste with C-y): "))))
        (when (and (stringp key) (not (string-empty-p (string-trim key))))
          (setq gemit--api-key-cache (string-trim key))))))

(defun gemit--api-key ()
  "Return the Gemini API key: setting, env, auth-source, then prompt."
  (gemit--api-key-for-setting gemit-api-key))

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

;;; Local llama.cpp API call

(defun gemit--url-retrieve-async
    (url timeout local-request-p method data headers callback)
  "Retrieve URL asynchronously, bounded by TIMEOUT when non-nil.
METHOD, DATA, and HEADERS configure the HTTP request.  LOCAL-REQUEST-P
disables redirects and bypasses proxies for loopback URLs.  CALLBACK receives
the status plist and response buffer; both are nil when no buffer exists."
  (let ((done nil)
        timer request-buffer)
    (cl-labels
        ((finish (status buffer)
           (unless done
             (setq done t
                   request-buffer (or buffer request-buffer))
             (when timer
               (cancel-timer timer)
               (setq timer nil))
             (unwind-protect
                 (condition-case err
                     (funcall callback status buffer)
                   (error
                    (message "gemit: Request callback failed: %s"
                             (error-message-string err))))
               (when (buffer-live-p request-buffer)
                 (kill-buffer request-buffer)))))
         (receive (status)
           (if done
               (when (buffer-live-p (current-buffer))
                 (kill-buffer (current-buffer)))
             (finish status (current-buffer))))
         (timeout-request ()
           (finish (list :error
                         (list 'error
                               (format "Request timed out after %s seconds"
                                       timeout)))
                   request-buffer)))
      (when timeout
        (setq timer (run-at-time timeout nil #'timeout-request)))
      (condition-case err
          (let ((url-request-method method)
                (url-request-data data)
                (url-request-extra-headers headers)
                (url-mime-accept-string gemit--json-content-type)
                (url-max-redirections (if local-request-p
                                          0
                                        url-max-redirections))
                (url-proxy-services
                 (if (and local-request-p (gemit--loopback-url-p url))
                     nil
                   url-proxy-services)))
            (setq request-buffer (url-retrieve url #'receive nil t))
            (unless done
              (unless (buffer-live-p request-buffer)
                (finish '(:error (error "URL request returned no buffer"))
                        nil)))
            (when (and done (buffer-live-p request-buffer))
              (kill-buffer request-buffer)))
        (quit
         (unless done
           (setq done t)
           (when timer
             (cancel-timer timer)
             (setq timer nil))
           (when (buffer-live-p request-buffer)
             (kill-buffer request-buffer)))
         (signal (car err) (cdr err)))
        (error
         (unless done
           (finish (list :error
                         (list 'error
                               (format "Could not start URL request: %s"
                                       (error-message-string err))))
                   nil)))))))

(defun gemit--gemini-handle-response (status callback)
  "Handle a Gemini response STATUS and call CALLBACK once."
  (let ((result
         (condition-case err
             (let ((request-error (plist-get status :error)))
               (if request-error
                   (let ((msg (gemit--api-error request-error)))
                     (when (gemit--invalid-key-error-p msg)
                       (setq gemit--api-key-cache nil))
                     (list nil msg))
                 (progn
                   (goto-char (or url-http-end-of-headers (point-min)))
                   (let* ((response (json-parse-buffer :object-type 'plist))
                          (text (gemit--response-text response)))
                     (if text
                         (list text nil)
                       (list nil
                             (or (plist-get (plist-get response :error) :message)
                                 "No response content from Gemini API")))))))
           (error (list nil (error-message-string err))))))
    (funcall callback (car result) (cadr result))))

(defun gemit--gemini-request-async (prompt system callback &optional config)
  "Send PROMPT and SYSTEM to Gemini API asynchronously.
CALLBACK receives (RESPONSE nil) on success or (nil ERROR-MSG) on failure.
CONFIG snapshots the model and key setting for a dispatched request."
  (let ((model (or (plist-get config :model) gemit-model))
        (key-setting (if (plist-member config :api-key-setting)
                         (plist-get config :api-key-setting)
                       gemit-api-key))
        (completed nil))
    (cl-labels ((complete (response error-msg)
                  (unless completed
                    (setq completed t)
                    (condition-case err
                        (funcall callback response error-msg)
                      (error
                       (message "gemit: Gemini callback failed: %s"
                                (error-message-string err)))))))
      (condition-case err
          (let ((api-key (gemit--api-key-for-setting key-setting)))
            (if (not api-key)
                (complete nil "Google Gemini API key not set")
              (gemit--url-retrieve-async
               (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent?key=%s"
                       model api-key)
               nil nil "POST"
               (encode-coding-string
                (json-serialize
                 (list :systemInstruction
                       (list :parts (vector (list :text system)))
                       :contents
                       (vector (list :parts (vector (list :text prompt))))))
                'utf-8)
               `(("content-type" . ,gemit--json-content-type))
               (lambda (status _buffer)
                  (gemit--gemini-handle-response status #'complete)))))
        (quit (complete nil "Gemini API key lookup cancelled"))
        (error
         (complete nil
                   (format "Gemini API key lookup failed: %s"
                            (error-message-string err))))))))

(defun gemit--normalize-local-url (url)
  "Return URL without trailing slashes and with exactly one final slash."
  (let ((root (replace-regexp-in-string "/+\\'" "" (string-trim url))))
    (concat root "/")))

(defun gemit--local-endpoint (root path)
  "Join normalized ROOT and slash-prefixed PATH into a request URL."
  (concat root (string-remove-prefix "/" path)))

(defun gemit--loopback-url-p (url)
  "Return non-nil if URL names a loopback host."
  (let* ((host (downcase (or (url-host (url-generic-parse-url url)) ""))))
    (or (member host '("localhost" "::1" "[::1]"))
        (string-match-p "\\`127\\(?:\\.[0-9]+\\)\\{3\\}\\'" host))))

(defun gemit--local-error-message (request-error)
  "Format a URL layer REQUEST-ERROR for a local server request."
  (cond
   ((and (eq (car-safe request-error) 'error)
         (stringp (cadr request-error)))
    (cadr request-error))
   ((and (eq (car-safe request-error) 'error)
         (eq (cadr request-error) 'http))
    (format "HTTP %s" (nth 2 request-error)))
   (t (error-message-string request-error))))

(defun gemit--local-json-response (status)
  "Parse STATUS as a local JSON response or signal an error."
  (when-let* ((request-error (plist-get status :error)))
    (error "%s" (gemit--local-error-message request-error)))
  (goto-char (or url-http-end-of-headers (point-min)))
  (json-parse-buffer :object-type 'plist))

(defun gemit--local-request-async
  (prompt system endpoint model availability-timeout generation-timeout callback)
  "Send PROMPT and SYSTEM to the llama.cpp server at ENDPOINT using MODEL.
Use AVAILABILITY-TIMEOUT for readiness/discovery and GENERATION-TIMEOUT
for completion; deliver a response or error to CALLBACK."
  (cl-labels
      ((fail (reason)
         (funcall callback nil reason))
       (request-json (url timeout method data headers next)
         (gemit--url-retrieve-async
          url timeout t method data headers
          (lambda (status buffer)
            (condition-case err
                (if (not (buffer-live-p buffer))
                    (error "Local server returned no response")
                  (with-current-buffer buffer
                    (funcall next (gemit--local-json-response status))))
              (error (fail (error-message-string err)))))))
       (generate (selected-model)
         (let* ((url (gemit--local-endpoint
                      endpoint gemit--local-completions-path))
                (data
                 (encode-coding-string
                  (json-serialize
                   (list :model selected-model
                         :messages
                         (vector (list :role "system" :content system)
                                 (list :role "user" :content prompt))
                         :stream :false))
                  'utf-8)))
           (request-json
            url generation-timeout "POST" data
            `(("content-type" . ,gemit--json-content-type))
            (lambda (response)
              (let* ((choices (plist-get response :choices))
                     (message (and (vectorp choices) (> (length choices) 0)
                                   (aref choices 0)))
                     (content (plist-get (plist-get message :message) :content)))
                (if (and (stringp content)
                         (not (string-empty-p (string-trim content))))
                    (funcall callback content nil)
                  (fail "Local server returned no nonblank message content")))))))
       (discover-model ()
         (request-json
          (gemit--local-endpoint endpoint gemit--local-models-path)
          availability-timeout "GET" nil nil
          (lambda (response)
            (let* ((data (plist-get response :data))
                   (ids (and (vectorp data)
                             (delq nil
                                   (mapcar
                                    (lambda (item)
                                      (let ((id (plist-get item :id)))
                                        (and (stringp id)
                                              (not (string-empty-p
                                                    (string-trim id)))
                                              (string-trim id))))
                                    (append data nil)))))
                   (ids (delete-dups ids)))
              (cond
               ((null ids)
                (fail "No usable model ID from /v1/models; configure gemit-local-model"))
               ((cdr ids)
                (fail "Multiple local model IDs are ambiguous; configure gemit-local-model"))
               (t (generate (car ids))))))))
       (check-health ()
         (request-json
          (gemit--local-endpoint endpoint gemit--local-health-path)
          availability-timeout "GET" nil nil
          (lambda (response)
            (if (equal (plist-get response :status) "ok")
                (if model
                    (if (stringp model)
                        (if (string-empty-p (string-trim model))
                            (fail "gemit-local-model must not be blank")
                          (generate (string-trim model)))
                      (fail "gemit-local-model must be a string or nil"))
                  (discover-model))
              (fail "Local llama.cpp server is not ready (expected status ok)"))))))
    (condition-case err
        (check-health)
      (error (fail (error-message-string err))))))

(defun gemit--request-async (prompt system callback)
  "Dispatch PROMPT and SYSTEM to the selected backend asynchronously.
CALLBACK receives (RESPONSE nil) on success or (nil ERROR-MSG) on failure."
  (let* ((backend gemit-backend)
         (local-url gemit-local-url)
         (local-model gemit-local-model)
         (availability-timeout gemit-local-availability-timeout)
         (generation-timeout gemit-local-generation-timeout)
         (gemini-config (list :model gemit-model
                              :api-key-setting gemit-api-key))
         (completed nil))
    (cl-labels
        ((complete (response error-msg)
           (unless completed
             (setq completed t)
             (condition-case err
                 (funcall callback response error-msg)
               (error
                (message "gemit: Request callback failed: %s"
                         (error-message-string err))))))
         (local-failed (reason)
           (if (eq backend 'auto)
               (progn
                 (message "gemit: Local llama.cpp request failed: %s" reason)
                 (condition-case err
                     (if (y-or-n-p
                          "Send the entire staged diff to Google Gemini? ")
                         (gemit--gemini-request-async
                          prompt system #'complete gemini-config)
                       (complete nil
                                 "Gemini fallback declined; staged diff was not sent"))
                   (quit
                    (complete nil
                              "Gemini fallback cancelled; staged diff was not sent"))
                   (error
                    (complete nil
                               (format "Gemini fallback confirmation failed; staged diff was not sent: %s"
                                       (error-message-string err))))))
             (complete nil reason))))
      (pcase backend
        ('gemini
         (gemit--gemini-request-async prompt system #'complete gemini-config))
        ((or 'auto 'local)
         (condition-case err
             (gemit--local-request-async
              prompt system (gemit--normalize-local-url local-url)
              local-model availability-timeout
              generation-timeout
              (lambda (response error-msg)
                (if response
                    (complete response nil)
                  (local-failed error-msg))))
           (error (local-failed (error-message-string err)))))
        (_ (complete nil (format "Unknown gemit-backend: %S" backend)))))))

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
