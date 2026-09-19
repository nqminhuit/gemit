;;; gemit-test.el --- Tests for gemit -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers gemit's pure helpers only: API key lookup, response parsing,
;; error reporting, message formatting, and the `gemit--generate' glue
;; with the network call stubbed out.  Nothing here touches the network
;; or needs Magit installed -- every path that would (`gemit--request-async',
;; `magit-git-output') is stubbed, since a real request needs an API key
;; with generation access, neither of which CI has.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gemit)

;; Neither is bound in a plain `-Q --batch' run (magit and url.el's
;; response buffers are never loaded there), so `let'-binding them without
;; this would create lexical bindings under `lexical-binding: t' --
;; invisible to the dynamic lookups in the code under test.
(defvar url-http-end-of-headers)
(defvar git-commit-summary-max-length)

;; `cl-letf' needs an existing function cell to stub; CI has no Magit, so
;; provide a fallback definition there.  Where Magit is installed this is
;; a no-op and the real function gets stubbed instead.
(unless (fboundp 'magit-git-output)
  (defun magit-git-output (&rest _)
    ""))

;;;; gemit--resolve-api-key

(ert-deftest gemit-test-resolve-api-key-nil ()
  (should (null (gemit--resolve-api-key nil))))

(ert-deftest gemit-test-resolve-api-key-string-passes-through ()
  (should (equal "AIza-key" (gemit--resolve-api-key "AIza-key"))))

(ert-deftest gemit-test-resolve-api-key-missing-path-passes-through ()
  ;; A string naming nothing is a (probably wrong) key, not a file.
  (should (equal "/no/such/file" (gemit--resolve-api-key "/no/such/file"))))

(ert-deftest gemit-test-resolve-api-key-file-is-read-and-trimmed ()
  (let ((f (make-temp-file "gemit-key")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "  AIza-key\n"))
          (should (equal "AIza-key" (gemit--resolve-api-key f))))
      (delete-file f))))

;;;; gemit--api-key lookup chain

(defmacro gemit-test--with-clean-env (&rest body)
  "Run BODY with no cached key and no GEMINI_API_KEY_GEMIT set."
  (declare (indent 0))
  `(let ((gemit--api-key-cache nil)
         (old-env (getenv "GEMINI_API_KEY_GEMIT")))
     (unwind-protect (progn (setenv "GEMINI_API_KEY_GEMIT") ,@body)
       (when old-env (setenv "GEMINI_API_KEY_GEMIT" old-env)))))

(ert-deftest gemit-test-api-key-custom-setting-wins ()
  (gemit-test--with-clean-env
    (let ((gemit-api-key "custom-key"))
      (should (equal "custom-key" (gemit--api-key))))))

(ert-deftest gemit-test-api-key-falls-back-to-env ()
  (gemit-test--with-clean-env
    (setenv "GEMINI_API_KEY_GEMIT" "env-key")
    (let ((gemit-api-key nil))
      (should (equal "env-key" (gemit--api-key))))))

(ert-deftest gemit-test-api-key-blank-values-never-prompt ()
  ;; Blank custom setting and blank env must both be skipped; reaching
  ;; `read-passwd' here is the failure.
  (gemit-test--with-clean-env
    (setenv "GEMINI_API_KEY_GEMIT" "   ")
    (let ((gemit-api-key "  "))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (error "must not prompt"))))
        (should (null (gemit--api-key)))))))

(ert-deftest gemit-test-api-key-prompted-value-is-cached ()
  (gemit-test--with-clean-env
    (let ((gemit-api-key nil)
          (calls 0))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq calls (1+ calls)) "typed-key")))
        (should (equal "typed-key" (gemit--api-key)))
        (should (equal "typed-key" (gemit--api-key)))
        (should (= 1 calls))))))

;;;; gemit--response-text

;; Fixtures below mirror `json-parse-buffer' output: JSON objects are
;; plists, JSON arrays are vectors.

(defconst gemit-test--ok-response
  '(:candidates [(:content (:parts [(:text "fix: Test")])
                 :finishReason "STOP")]))

(ert-deftest gemit-test-response-text-happy-path ()
  (should (equal "fix: Test"
                 (gemit--response-text gemit-test--ok-response))))

(ert-deftest gemit-test-response-text-no-candidates ()
  (should (null (gemit--response-text '(:promptFeedback (:blockReason "X"))))))

(ert-deftest gemit-test-response-text-error-body ()
  (should (null (gemit--response-text
                 '(:error (:code 403 :message "denied" :status "PERMISSION_DENIED"))))))

;;;; gemit--api-error

(ert-deftest gemit-test-api-error-prefers-api-message ()
  (with-temp-buffer
    (insert "{\"error\": {\"code\": 403, \"message\": \"denied\", \"status\": \"PERMISSION_DENIED\"}}")
    (let ((url-http-end-of-headers (point-min)))
      (should (equal "denied" (gemit--api-error '(error http 403)))))))

(ert-deftest gemit-test-api-error-falls-back-without-headers ()
  ;; No parseable response (DNS failure and friends): report the status.
  (let ((url-http-end-of-headers nil))
    (should (equal "Connection error: (error http 403)"
                   (gemit--api-error '(error http 403))))))

;;;; gemit--format-commit-message

(ert-deftest gemit-test-format-commit-message-wraps-long-subject ()
  (let ((git-commit-summary-max-length 20))
    (let ((out (gemit--format-commit-message "fix: a very long subject line here")))
      (should (string-match-p "fix:" out))
      (should (> (length (split-string out "\n")) 1))
      (should (<= (length (car (split-string out "\n"))) 20)))))

(ert-deftest gemit-test-format-commit-message-keeps-short-message ()
  (let ((git-commit-summary-max-length 72))
    (should (equal "fix: Short"
                   (gemit--format-commit-message "fix: Short")))))

;;;; gemit--generate glue (request stubbed, no network)

(ert-deftest gemit-test-generate-empty-diff-errors ()
  (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) "  \n")))
    (should-error (gemit--generate #'ignore) :type 'user-error)))

(ert-deftest gemit-test-generate-delivers-formatted-message ()
  (let ((seen nil)
        (git-commit-summary-max-length 72))
    (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) "diff"))
              ((symbol-function 'gemit--request-async)
               (lambda (_prompt _system cb) (funcall cb "fix: Short" nil))))
      (gemit--generate (lambda (msg) (setq seen msg))))
    (should (equal "fix: Short" seen))))

(ert-deftest gemit-test-generate-reports-api-error ()
  (let ((msgs '()))
    (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) "diff"))
              ((symbol-function 'gemit--request-async)
               (lambda (_prompt _system cb) (funcall cb nil "denied")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (gemit--generate #'ignore))
    (should (= 2 (length msgs)))
    (should (string-match-p "gemit: denied" (car msgs)))))

(provide 'gemit-test)
;;; gemit-test.el ends here
