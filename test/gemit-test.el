;;; gemit-test.el --- Tests for gemit -*- lexical-binding: t; -*-

;;; Commentary:

;; Covers helpers and local/Gemini request flows with deterministic HTTP
;; and timer stubs. Nothing touches the network or needs Magit installed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'gemit)

;; Neither is bound in a plain `-Q --batch' run (magit and url.el's
;; response buffers are never loaded there), so `let'-binding them without
;; this would create lexical bindings under `lexical-binding: t' --
;; invisible to the dynamic lookups in the code under test.
(defvar url-http-end-of-headers)
(defvar url-request-method)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-max-redirections)
(defvar url-proxy-services)
(defvar url-proxy-locator)
(defvar git-commit-summary-max-length)

;; `cl-letf' needs an existing function cell to stub; CI has no Magit, so
;; provide a fallback definition there.  Where Magit is installed this is
;; a no-op and the real function gets stubbed instead.
(unless (fboundp 'magit-git-output)
  (defun magit-git-output (&rest _)
    gemit-test--empty-string))

(defconst gemit-test--local-root "http://127.0.0.1:8080")
(defconst gemit-test--staged-diff "entire staged diff")
(defconst gemit-test--system-prompt "system")
(defconst gemit-test--diff "diff")
(defconst gemit-test--empty-string "")
(defconst gemit-test--api-key "AIza-key")
(defconst gemit-test--typed-key "typed-key")
(defconst gemit-test--denied-message "denied")
(defconst gemit-test--short-message "fix: Short")
(defconst gemit-test--cloud-message "fix: cloud")
(defconst gemit-test--health-path "/health")
(defconst gemit-test--chat-completions-path "/v1/chat/completions")
(defconst gemit-test--configured-model "configured")
(defconst gemit-test--connection-error "connection refused")
(defconst gemit-test--health-body (json-serialize '(:status "ok")))
(defconst gemit-test--models-body
  (json-serialize (list :data (vector (list :id "loaded-model")))))
(defconst gemit-test--local-content "feat: café")
(defconst gemit-test--completion-body
  (json-serialize
   (list :choices (vector (list :message (list :content gemit-test--local-content))))))
(defconst gemit-test--gemini-body
  (json-serialize
   (list :candidates
          (vector (list :content (list :parts (vector (list :text gemit-test--cloud-message))))))))

(defmacro gemit-test--with-http-responses (responses records &rest body)
  "Run BODY with URL retrieval answered by RESPONSES, recording requests."
  (declare (indent 2))
  `(let ((queue ,responses))
     (cl-letf (((symbol-function 'url-retrieve)
                (lambda (url callback &rest _)
                  (push (list :url url
                              :method url-request-method
                              :data url-request-data
                              :headers url-request-extra-headers
                              :max-redirections url-max-redirections
                              :proxies url-proxy-services)
                        ,records)
                  (let ((entry (pop queue)))
                    (unless entry
                      (error "No HTTP test response queued for %s" url))
                    (when (plist-get entry :startup-error)
                      (error "simulated URL startup error"))
                    (unless (plist-get entry :return-nil)
                      (let ((buffer (generate-new-buffer " *gemit-http-test*")))
                        (with-current-buffer buffer
                          (insert (decode-coding-string
                                    (or (plist-get entry :body) gemit-test--empty-string) 'utf-8))
                          (let ((url-http-end-of-headers (point-min)))
                            (funcall callback (plist-get entry :status))))
                        buffer))))))
       ,@body)))

(defmacro gemit-test--with-fake-timers (timers &rest body)
  "Run BODY with cancellable fake timers recorded in TIMERS."
  (declare (indent 1))
  `(let (,timers)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (seconds _repeat function &rest arguments)
                  (let ((timer (vector seconds function arguments nil)))
                    (push timer ,timers)
                    timer)))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (aset timer 3 t))))
       ,@body)))

(defun gemit-test--fixture (body &optional status)
  "Return an HTTP test response with BODY and optional STATUS."
  (list :body body :status status))

(defun gemit-test--json-request-body (record)
  "Parse the request JSON body from RECORD."
  (json-parse-string
   (decode-coding-string (plist-get record :data) 'utf-8)
   :object-type 'plist :array-type 'array :false-object :false))

;;;; gemit--resolve-api-key

(ert-deftest gemit-test-resolve-api-key-nil ()
  (should (null (gemit--resolve-api-key nil))))

(ert-deftest gemit-test-resolve-api-key-string-passes-through ()
  (should (equal gemit-test--api-key (gemit--resolve-api-key gemit-test--api-key))))

(ert-deftest gemit-test-resolve-api-key-missing-path-passes-through ()
  ;; A string naming nothing is a (probably wrong) key, not a file.
  (should (equal "/no/such/file" (gemit--resolve-api-key "/no/such/file"))))

(ert-deftest gemit-test-resolve-api-key-file-is-read-and-trimmed ()
  (let ((f (make-temp-file "gemit-key")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "  AIza-key\n"))
          (should (equal gemit-test--api-key (gemit--resolve-api-key f))))
      (delete-file f))))

;;;; gemit--api-key lookup chain

(defmacro gemit-test--with-clean-env (&rest body)
  "Run BODY with no cached key and no API-key environment variable set."
  (declare (indent 0))
  `(let ((gemit--api-key-cache nil)
         (old-env (getenv gemit--api-key-env-var)))
     (unwind-protect (progn (setenv gemit--api-key-env-var) ,@body)
       (when old-env (setenv gemit--api-key-env-var old-env)))))

(ert-deftest gemit-test-api-key-custom-setting-wins ()
  (gemit-test--with-clean-env
    (let ((gemit-api-key "custom-key"))
      (should (equal "custom-key" (gemit--api-key))))))

(ert-deftest gemit-test-api-key-falls-back-to-env ()
  (gemit-test--with-clean-env
    (setenv gemit--api-key-env-var "env-key")
    (let ((gemit-api-key nil))
      (should (equal "env-key" (gemit--api-key))))))

(ert-deftest gemit-test-api-key-blank-values-never-prompt ()
  ;; Blank custom setting and blank env must both be skipped; reaching
  ;; `read-passwd' here is the failure.
  (gemit-test--with-clean-env
    (setenv gemit--api-key-env-var "   ")
    (let ((gemit-api-key "  "))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (error "must not prompt"))))
        (should (null (gemit--api-key)))))))

(ert-deftest gemit-test-api-key-prompted-value-is-cached ()
  (gemit-test--with-clean-env
    (let ((gemit-api-key nil)
          (calls 0))
      (cl-letf (((symbol-function 'read-passwd)
                  (lambda (&rest _) (setq calls (1+ calls)) gemit-test--typed-key)))
        (should (equal gemit-test--typed-key (gemit--api-key)))
        (should (equal gemit-test--typed-key (gemit--api-key)))
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
                  `(:error (:code 403 :message ,gemit-test--denied-message
                            :status "PERMISSION_DENIED"))))))

;;;; gemit--api-error

(ert-deftest gemit-test-api-error-prefers-api-message ()
  (with-temp-buffer
    (insert (json-serialize
             (list :error (list :code 403 :message gemit-test--denied-message
                                :status "PERMISSION_DENIED"))))
    (let ((url-http-end-of-headers (point-min)))
      (should (equal gemit-test--denied-message
                     (gemit--api-error '(error http 403)))))))

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
    (should (equal gemit-test--short-message
                   (gemit--format-commit-message gemit-test--short-message)))))

;;;; gemit--generate glue (request stubbed, no network)

(ert-deftest gemit-test-generate-empty-diff-errors ()
  (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) "  \n")))
    (should-error (gemit--generate #'ignore) :type 'user-error)))

(ert-deftest gemit-test-generate-delivers-formatted-message ()
  (let ((seen nil)
        (git-commit-summary-max-length 72))
    (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) gemit-test--diff))
              ((symbol-function 'gemit--request-async)
               (lambda (_prompt _system cb) (funcall cb gemit-test--short-message nil))))
      (gemit--generate (lambda (msg) (setq seen msg))))
    (should (equal gemit-test--short-message seen))))

(ert-deftest gemit-test-generate-reports-api-error ()
  (let ((msgs '()))
    (cl-letf (((symbol-function 'magit-git-output) (lambda (&rest _) gemit-test--diff))
              ((symbol-function 'gemit--request-async)
               (lambda (_prompt _system cb)
                 (funcall cb nil gemit-test--denied-message)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) msgs))))
      (gemit--generate #'ignore))
    (should (= 2 (length msgs)))
    (should (string-match-p "gemit: denied" (car msgs)))))

;;;; gemit--invalid-key-error-p and cache eviction

(ert-deftest gemit-test-invalid-key-matches-rejections ()
  (should (gemit--invalid-key-error-p
           "API key not valid. Please pass a valid API key."))
  (should (gemit--invalid-key-error-p "API_KEY_INVALID"))
  (should (gemit--invalid-key-error-p
           "API key expired. Please renew the API key."))
  (should (gemit--invalid-key-error-p
           "API keys are not supported by this API.")))

(ert-deftest gemit-test-invalid-key-ignores-other-failures ()
  ;; A restricted project or quota failure must NOT evict a good key.
  (should-not (gemit--invalid-key-error-p
               "Your project has been denied access. Please contact support."))
  (should-not (gemit--invalid-key-error-p "No response content from Gemini API"))
  (should-not (gemit--invalid-key-error-p nil)))

(defmacro gemit-test--with-stubbed-retrieve (body-message &rest body)
  "Run BODY with `url-retrieve' answering BODY-MESSAGE as an API error."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'url-retrieve)
              (lambda (_url callback &rest _)
                (with-temp-buffer
                  (insert (format "{\"error\": {\"message\": %S}}" ,body-message))
                  (let ((url-http-end-of-headers (point-min)))
                    (funcall callback '(:error (error http 400)))))))
             ((symbol-function 'read-passwd)
              (lambda (&rest _) (error "must not prompt"))))
     ,@body))

(ert-deftest gemit-test-rejected-key-evicts-cache ()
  (gemit-test--with-clean-env
    (let ((gemit--api-key-cache "bad-key")
          (gemit-api-key nil)
          (seen nil))
      (gemit-test--with-stubbed-retrieve "API key not valid. Please pass a valid API key."
        (gemit--gemini-request-async gemit-test--diff gemit-test--system-prompt
          (lambda (response err) (setq seen (or response err)))))
      (should (null gemit--api-key-cache))
      (should (string-match-p "not valid" seen)))))

(ert-deftest gemit-test-denied-project-keeps-cache ()
  (gemit-test--with-clean-env
    (let ((gemit--api-key-cache "good-key")
          (gemit-api-key nil)
          (seen nil))
      (gemit-test--with-stubbed-retrieve "Your project has been denied access. Please contact support."
        (gemit--gemini-request-async gemit-test--diff gemit-test--system-prompt
          (lambda (response err) (setq seen (or response err)))))
      (should (equal "good-key" gemit--api-key-cache))
      (should (string-match-p "denied access" seen)))))

;;;; Local llama.cpp requests

(ert-deftest gemit-test-local-first-defaults-are-configurable ()
  (should (eq 'auto (default-value 'gemit-backend)))
  (should (equal "http://127.0.0.1:8080" (default-value 'gemit-local-url)))
  (should-not (default-value 'gemit-local-model))
  (should (= 2 (default-value 'gemit-local-availability-timeout)))
  (should (= 120 (default-value 'gemit-local-generation-timeout)))
  (should (equal "gemini-flash-lite-latest" (default-value 'gemit-model))))

(ert-deftest gemit-test-local-success-discovers-model-and-sends-json-false ()
  (let ((records nil)
        (result nil)
        (gemit-backend 'local)
        (gemit-local-url (concat gemit-test--local-root "///"))
        (gemit-local-model nil))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture gemit-test--health-body)
              (gemit-test--fixture gemit-test--models-body)
              (gemit-test--fixture gemit-test--completion-body))
        records
      (cl-letf (((symbol-function 'gemit--api-key-for-setting)
                 (lambda (&rest _) (error "local success must not look up a Gemini key"))))
        (gemit--request-async
         "diff — café" "system λ"
         (lambda (response error-msg) (setq result (list response error-msg))))))
    (should (equal (list gemit-test--local-content nil) result))
    (let* ((requests (reverse records))
           (completion (nth 2 requests))
           (payload (gemit-test--json-request-body completion))
           (messages (plist-get payload :messages)))
      (should (equal (mapcar (lambda (request) (plist-get request :url)) requests)
                     (list (concat gemit-test--local-root gemit-test--health-path)
                           (concat gemit-test--local-root "/v1/models")
                           (concat gemit-test--local-root
                                   gemit-test--chat-completions-path))))
      (should (equal "GET" (plist-get (car requests) :method)))
      (should (equal "POST" (plist-get completion :method)))
      (should (equal '(("content-type" . "application/json"))
                     (plist-get completion :headers)))
      (should (equal 0 (plist-get completion :max-redirections)))
      (should-not (plist-get completion :proxies))
      (should (equal "loaded-model" (plist-get payload :model)))
      (should (eq :false (plist-get payload :stream)))
      (should (equal "system λ" (plist-get (aref messages 0) :content)))
      (should (equal "diff — café" (plist-get (aref messages 1) :content))))))

(ert-deftest gemit-test-real-loopback-transport-bypasses-proxies ()
  (require 'url-http)
  (let* ((proxy-url "http://proxy.example.invalid:8888")
         (process-environment (copy-sequence process-environment))
         (url-setup-done t)
         (url-retrieve-number-of-calls 1)
         (url-proxy-services nil)
         (selected-proxy 'unset)
         (custom-locator-calls 0))
    (setenv "HTTP_PROXY" proxy-url)
    (setenv "http_proxy" proxy-url)
    (setenv "NO_PROXY" nil)
    (setenv "no_proxy" nil)
    (cl-letf (((symbol-function 'url-http-find-free-connection)
               (lambda (&rest _)
                 (setq selected-proxy url-using-proxy)
                 nil)))
      (let ((url-proxy-locator #'url-default-find-proxy-for-url))
        (gemit--url-retrieve-async
         (concat gemit-test--local-root gemit-test--health-path)
         nil t "GET" nil nil (lambda (&rest _) nil)))
      (should-not selected-proxy)
      (let ((url-proxy-locator
             (lambda (_url _host)
               (setq custom-locator-calls (1+ custom-locator-calls))
               "DIRECT")))
        (gemit--url-retrieve-async
         (concat gemit-test--local-root gemit-test--health-path)
         nil t "GET" nil nil (lambda (&rest _) nil)))
      (should (= 0 custom-locator-calls))
      (let ((url-proxy-locator
             (lambda (_url _host)
               (setq custom-locator-calls (1+ custom-locator-calls))
               "DIRECT")))
        (gemit--url-retrieve-async
         "http://example.invalid/health"
         nil nil "GET" nil nil (lambda (&rest _) nil)))
      (should (> custom-locator-calls 0)))))

(ert-deftest gemit-test-local-configured-model-skips-discovery ()
  (let ((records nil)
        (result nil)
        (gemit-backend 'local)
        (gemit-local-model "explicit-alias"))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture gemit-test--health-body)
              (gemit-test--fixture gemit-test--completion-body))
        records
      (gemit--request-async gemit-test--diff gemit-test--system-prompt
                            (lambda (response error-msg)
                              (setq result (list response error-msg)))))
    (should (equal (list gemit-test--local-content nil) result))
    (should (= 2 (length records)))
    (should (equal (concat gemit-test--local-root gemit-test--health-path)
                   (plist-get (car (last records)) :url)))
    (should (equal "explicit-alias"
                   (plist-get (gemit-test--json-request-body (car records)) :model)))))

(ert-deftest gemit-test-local-failures-are-reported-without-cloud ()
  (let ((cases
         (list
          (list (list (gemit-test--fixture "{}" '(:error (error http 503))))
                nil "HTTP 503")
          (list (list (gemit-test--fixture (json-serialize '(:status "loading"))))
                nil "not ready")
          (list (list (gemit-test--fixture "{"))
                nil "JSON")
          (list (list (gemit-test--fixture gemit-test--health-body)
                      (gemit-test--fixture (json-serialize '(:data []))))
                nil "No usable model ID")
          (list (list (gemit-test--fixture gemit-test--health-body)
                      (gemit-test--fixture
                       (json-serialize
                        (list :data (vector (list :id "one") (list :id "two"))))))
                nil "Multiple local model IDs")
          (list (list (gemit-test--fixture gemit-test--health-body)
                      (gemit-test--fixture
                       (json-serialize
                        (list :choices (vector (list :message (list :content "  \n")))))))
                gemit-test--configured-model "nonblank")
          (list (list (gemit-test--fixture gemit-test--health-body)
                      (gemit-test--fixture
                       (json-serialize
                        (list :choices
                              (vector (list :message
                                            (list :reasoning "reasoning-only"
                                                  :content 42)))))))
                gemit-test--configured-model "nonblank")
          (list (list (gemit-test--fixture gemit-test--health-body)
                      (gemit-test--fixture gemit-test--completion-body
                                           `(:error (error ,gemit-test--connection-error))))
                gemit-test--configured-model gemit-test--connection-error))))
    (dolist (case cases)
      (let ((records nil)
            (result nil)
            (gemit-backend 'local)
            (gemit-local-model (nth 1 case)))
        (gemit-test--with-http-responses (car case) records
          (cl-letf (((symbol-function 'gemit--api-key-for-setting)
                     (lambda (&rest _) (error "must not look up cloud key")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "local backend must not ask"))))
            (gemit--request-async
             gemit-test--diff gemit-test--system-prompt
             (lambda (response error-msg)
               (setq result (list response error-msg))))))
        (should (null (car result)))
        (should (string-match-p (nth 2 case)
                                (or (cadr result) gemit-test--empty-string)))))))

(ert-deftest gemit-test-local-connection-failure-skips-model-discovery ()
  (let ((records nil)
        (result nil)
        (gemit-backend 'local))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture nil `(:error (error ,gemit-test--connection-error))))
        records
      (gemit--request-async gemit-test--diff gemit-test--system-prompt
                            (lambda (response error-msg)
                              (setq result (list response error-msg)))))
    (should (string-match-p gemit-test--connection-error (cadr result)))
    (should (= 1 (length records)))))

;;;; Backend selection and consent

(ert-deftest gemit-test-auto-refusal-is-per-request-and-never-looks-up-key ()
  (let ((records nil)
        (results nil)
        (questions nil)
        (gemini-calls 0)
        (key-lookups 0)
        (answers '(nil t))
        (gemit-backend 'auto)
        (gemit-local-model gemit-test--configured-model))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture nil '(:error (error http 503)))
              (gemit-test--fixture nil '(:error (error http 503))))
        records
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt questions)
                   (pop answers)))
                ((symbol-function 'gemit--api-key-for-setting)
                 (lambda (&rest _) (setq key-lookups (1+ key-lookups))
                   (error "refusal must not look up Gemini key")))
                ((symbol-function 'gemit--gemini-request-async)
                 (lambda (_prompt _system callback &optional _config)
                   (setq gemini-calls (1+ gemini-calls))
                   (funcall callback "cloud" nil))))
        (dotimes (_ 2)
          (gemit--request-async
           gemit-test--staged-diff gemit-test--system-prompt
           (lambda (response error-msg)
             (push (list response error-msg) results)))))
    (should (= 2 (length questions)))
    (should (cl-every (lambda (prompt)
                        (and (string-match-p gemit-test--staged-diff prompt)
                             (string-match-p "Google Gemini" prompt)))
                      questions))
    (should (= 1 gemini-calls))
    (should (= 0 key-lookups))
    (should (= 2 (length records)))
    (should (member "Gemini fallback declined; staged diff was not sent"
                    (mapcar #'cadr results))))))

(ert-deftest gemit-test-auto-consent-sends-entire-diff-to-gemini ()
  (let ((records nil)
        (result nil)
        (asked nil)
        (gemit-backend 'auto)
        (gemit-local-model gemit-test--configured-model)
        (gemit--api-key-cache "cached-key"))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture nil '(:error (error http 503)))
              (gemit-test--fixture gemit-test--gemini-body))
        records
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (prompt) (setq asked prompt) t)))
        (gemit--request-async
          gemit-test--staged-diff gemit-test--system-prompt
         (lambda (response error-msg) (setq result (list response error-msg)))))
    (should (equal (list gemit-test--cloud-message nil) result))
    (should (string-match-p gemit-test--staged-diff asked))
    (let ((requests (reverse records)))
      (should (= 2 (length requests)))
      (should (string-prefix-p gemit-test--local-root
                               (plist-get (car requests) :url)))
      (should (string-prefix-p "https://generativelanguage.googleapis.com/"
                               (plist-get (cadr requests) :url)))
      (should (string-match-p gemit-test--staged-diff
                              (decode-coding-string
                                (plist-get (cadr requests) :data) 'utf-8)))))))

(ert-deftest gemit-test-gemini-backend-bypasses-local-check-and-consent ()
  (let ((records nil)
        (result nil)
        (gemit-backend 'gemini)
        (gemit-local-url nil)
        (gemit--api-key-cache "cached-key"))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture gemit-test--gemini-body))
        records
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (error "gemini mode must not ask"))))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg) (setq result (list response error-msg)))))
    (should (equal (list gemit-test--cloud-message nil) result))
    (should (= 1 (length records)))
    (should (string-prefix-p "https://generativelanguage.googleapis.com/"
                             (plist-get (car records) :url))))))

;;;; Timeout cleanup

(defun gemit-test--running-request-buffer (name query-count)
  "Return a buffer with a live network-free process and query counter.
NAME labels the test buffer; QUERY-COUNT is incremented if killing prompts."
  (let ((buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions
                  (list (lambda () (setcar query-count (1+ (car query-count)))
                          t)))
      (make-pipe-process :name name :buffer buffer :noquery t))
    buffer))

(ert-deftest gemit-test-local-timeout-aborts-process-without-querying ()
  (let ((timers nil)
        (result nil)
        (calls 0)
        (calls-during-startup nil)
        (query-count (list 0))
        (gemit-backend 'local)
        (gemit-local-model gemit-test--configured-model)
        (gemit-local-availability-timeout 0.25)
        pending-callback request-buffer request-process)
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (setq pending-callback callback
                         request-buffer
                         (gemit-test--running-request-buffer
                          " *gemit-running-timeout*" query-count)
                         request-process (get-buffer-process request-buffer))
                   (funcall (aref (car timers) 1))
                   (setq calls-during-startup calls)
                   request-buffer)))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg)
           (setq calls (1+ calls) result (list response error-msg)))))
      (should (= 0 calls-during-startup))
      (should (= 1 calls))
      (should (string-match-p "timed out" (cadr result)))
      (should (aref (car timers) 3))
      (should-not (buffer-live-p request-buffer))
      (should-not (process-live-p request-process))
      (should (= 0 (car query-count)))
      (let ((late-buffer (generate-new-buffer " *gemit-running-late*")))
        (with-current-buffer late-buffer
          (let ((url-http-end-of-headers (point-min)))
            (funcall pending-callback nil)))
        (should-not (buffer-live-p late-buffer)))
      (should (= 1 calls)))))

(ert-deftest gemit-test-auto-timeout-cleans-before-fallback-consent ()
  (let ((timers nil)
        (result nil)
        (calls 0)
        (prompt-calls 0)
        (key-lookups 0)
        (query-count (list 0))
        request-buffer request-process
        (gemit-backend 'auto)
        (gemit-local-model gemit-test--configured-model)
        (gemit-local-availability-timeout 0.25))
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url _callback &rest _)
                   (setq request-buffer
                         (gemit-test--running-request-buffer
                          " *gemit-auto-timeout*" query-count))
                   request-buffer))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _)
                   (setq prompt-calls (1+ prompt-calls))
                   (should-not (buffer-live-p request-buffer))
                   (should-not (process-live-p request-process))
                   nil))
                ((symbol-function 'gemit--api-key-for-setting)
                 (lambda (&rest _)
                   (setq key-lookups (1+ key-lookups))
                   (error "Declined fallback must not look up a key"))))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg)
           (setq calls (1+ calls) result (list response error-msg))))
      (setq request-process (get-buffer-process request-buffer))
      (should (process-live-p request-process))
      (funcall (aref (car timers) 1))
      (should (= 1 prompt-calls))
      (should (= 0 key-lookups))
      (should (= 1 calls))
      (should (string-match-p "fallback declined" (cadr result)))
      (should-not (buffer-live-p request-buffer))
      (should-not (process-live-p request-process))
      (should (= 0 (car query-count)))))))

(ert-deftest gemit-test-local-generation-timeout-aborts-process-without-querying ()
  (let ((timers nil)
        (result nil)
        (calls 0)
        (query-count (list 0))
        pending-callback generation-buffer generation-process
        (gemit-backend 'local)
        (gemit-local-model nil)
        (gemit-local-generation-timeout 37))
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (if (string-suffix-p gemit-test--chat-completions-path url)
                       (progn
                         (setq pending-callback callback
                               generation-buffer
                                (gemit-test--running-request-buffer
                                 " *gemit-running-generation*" query-count))
                         generation-buffer)
                     (let ((buffer (generate-new-buffer " *gemit-fast-http*")))
                       (with-current-buffer buffer
                         (insert (if (string-suffix-p gemit-test--health-path url)
                                     gemit-test--health-body
                                   gemit-test--models-body))
                         (let ((url-http-end-of-headers (point-min)))
                           (funcall callback nil)))
                       buffer)))))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg)
           (setq calls (1+ calls) result (list response error-msg)))))
      (setq generation-process (get-buffer-process generation-buffer))
      (should (= 3 (length timers)))
      (should (process-live-p generation-process))
      (funcall (aref (car timers) 1))
      (should (= 1 calls))
      (should (string-match-p "timed out" (cadr result)))
      (should-not (buffer-live-p generation-buffer))
      (should-not (process-live-p generation-process))
      (should (= 0 (car query-count)))
      (let ((late-buffer (generate-new-buffer " *gemit-generation-late*")))
        (with-current-buffer late-buffer
          (let ((url-http-end-of-headers (point-min)))
            (funcall pending-callback nil)))
        (should-not (buffer-live-p late-buffer)))
      (should (= 1 calls)))))

(ert-deftest gemit-test-local-availability-timeout-completes-once-and-cleans-late-buffer ()
  (let ((timers nil)
        (result nil)
        (calls 0)
        pending-callback
        request-buffer
        (gemit-backend 'local)
        (gemit-local-model gemit-test--configured-model)
        (gemit-local-availability-timeout 0.25))
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (_url callback &rest _)
                   (setq pending-callback callback
                         request-buffer (generate-new-buffer " *gemit-timeout*"))
                   request-buffer)))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg)
           (setq calls (1+ calls) result (list response error-msg)))))
      (should (= 0.25 (aref (car timers) 0)))
      (funcall (aref (car timers) 1))
      (should (= 1 calls))
      (should (string-match-p "timed out" (cadr result)))
      (should (aref (car timers) 3))
      (should-not (buffer-live-p request-buffer))
      (let ((late-buffer (generate-new-buffer " *gemit-late*")))
        (with-current-buffer late-buffer
          (insert gemit-test--health-body)
          (let ((url-http-end-of-headers (point-min)))
            (funcall pending-callback nil)))
        (should-not (buffer-live-p late-buffer)))
      (should (= 1 calls)))))

(ert-deftest gemit-test-local-generation-timeout-bounds-completion-request ()
  (let ((timers nil)
        (records nil)
        (result nil)
        (calls 0)
        pending-callback
        generation-buffer
        (gemit-backend 'local)
        (gemit-local-model nil)
        (gemit-local-generation-timeout 37))
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push url records)
                    (if (string-suffix-p gemit-test--chat-completions-path url)
                       (setq pending-callback callback
                             generation-buffer
                             (generate-new-buffer " *gemit-generation*"))
                     (let ((buffer (generate-new-buffer " *gemit-fast-http*")))
                       (with-current-buffer buffer
                          (insert (if (string-suffix-p gemit-test--health-path url)
                                     gemit-test--health-body
                                   gemit-test--models-body))
                         (let ((url-http-end-of-headers (point-min)))
                           (funcall callback nil)))
                        buffer)))))
        (gemit--request-async
         gemit-test--diff gemit-test--system-prompt
         (lambda (response error-msg)
           (setq calls (1+ calls) result (list response error-msg)))))
      (should (= 3 (length timers)))
      (should (= 37 (aref (car timers) 0)))
      (funcall (aref (car timers) 1))
      (should (= 1 calls))
      (should (string-match-p "timed out" (cadr result)))
      (should-not (buffer-live-p generation-buffer))
      (should (= 3 (length records))))))

(ert-deftest gemit-test-url-startup-errors-and-nil-returns-complete-once ()
  (dolist (case (list (cons (list :startup-error t)
                           "Could not start URL request: simulated URL startup error")
                     (cons (list :return-nil t)
                           "URL request returned no buffer")))
    (let ((fixture (car case))
          (expected (cdr case))
          (records nil)
          (timers nil)
          (result nil)
          (calls 0)
          (gemit-backend 'local)
          (gemit-local-model gemit-test--configured-model))
      (gemit-test--with-fake-timers timers
        (gemit-test--with-http-responses (list fixture) records
          (gemit--request-async
           gemit-test--staged-diff gemit-test--system-prompt
           (lambda (response error-msg)
             (setq calls (1+ calls) result (list response error-msg)))))
         (should (= 1 calls))
         (should (null (car result)))
         (should (equal expected (cadr result)))
         (should (aref (car timers) 3))
         (should (= 1 (length records)))))))

(ert-deftest gemit-test-local-callback-exception-does-not-repeat-completion ()
  (let ((records nil)
        (calls 0)
        (gemit-backend 'local)
        (gemit-local-model gemit-test--configured-model))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture gemit-test--health-body)
              (gemit-test--fixture gemit-test--completion-body))
        records
      (gemit--request-async
       gemit-test--staged-diff gemit-test--system-prompt
       (lambda (_response _error-msg)
         (setq calls (1+ calls))
         (error "simulated user callback failure"))))
    (should (= 1 calls))))

(ert-deftest gemit-test-c-g-at-fallback-never-starts-gemini ()
  (let ((records nil)
        (result nil)
        (key-lookups 0)
        (gemini-calls 0)
        (gemit-backend 'auto)
        (gemit-local-model gemit-test--configured-model))
    (gemit-test--with-http-responses
        (list (gemit-test--fixture nil '(:error (error http 503))))
        records
      (cl-letf (((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (signal 'quit nil)))
                ((symbol-function 'gemit--api-key-for-setting)
                 (lambda (&rest _) (setq key-lookups (1+ key-lookups))))
                ((symbol-function 'gemit--gemini-request-async)
                 (lambda (&rest _) (setq gemini-calls (1+ gemini-calls)))))
        (gemit--request-async
         gemit-test--staged-diff gemit-test--system-prompt
         (lambda (response error-msg) (setq result (list response error-msg)))))
    (should (equal "Gemini fallback cancelled; staged diff was not sent"
                   (cadr result)))
    (should (= 0 key-lookups))
    (should (= 0 gemini-calls))
     (should (= 1 (length records))))))

(ert-deftest gemit-test-gemini-key-lookup-errors-use-one-callback ()
  (let ((result nil)
        (calls 0)
        (gemit-backend 'gemini))
    (cl-letf (((symbol-function 'gemit--api-key-for-setting)
               (lambda (&rest _) (error "key source failed")))
              ((symbol-function 'url-retrieve)
               (lambda (&rest _) (error "must not start remote request"))))
      (gemit--request-async
       gemit-test--staged-diff gemit-test--system-prompt
       (lambda (response error-msg)
         (setq calls (1+ calls) result (list response error-msg)))))
    (should (= 1 calls))
    (should (string-match-p "key lookup failed" (cadr result)))))

(ert-deftest gemit-test-pending-request-uses-configuration-snapshot ()
  (let ((records nil)
        (timers nil)
        (result nil)
        pending-health
        health-buffer
        (gemit-backend 'local)
        (gemit-local-url gemit-test--local-root)
        (gemit-local-model "captured-model")
        (gemit-local-availability-timeout 3)
        (gemit-local-generation-timeout 45))
    (gemit-test--with-fake-timers timers
      (cl-letf (((symbol-function 'url-retrieve)
                 (lambda (url callback &rest _)
                   (push (list url url-request-method url-request-data) records)
                    (if (string-suffix-p gemit-test--health-path url)
                       (setq pending-health callback
                             health-buffer (generate-new-buffer " *gemit-pending-health*"))
                     (let ((buffer (generate-new-buffer " *gemit-snapshot-result*")))
                       (with-current-buffer buffer
                         (insert (decode-coding-string gemit-test--completion-body 'utf-8))
                         (let ((url-http-end-of-headers (point-min)))
                           (funcall callback nil)))
                        buffer)))))
        (gemit--request-async
         gemit-test--staged-diff gemit-test--system-prompt
         (lambda (response error-msg) (setq result (list response error-msg))))
      (setq gemit-backend 'gemini
            gemit-local-url "http://elsewhere.invalid:9999"
            gemit-local-model "changed-model"
            gemit-local-generation-timeout 1)
      (with-current-buffer health-buffer
        (insert (decode-coding-string gemit-test--health-body 'utf-8))
        (let ((url-http-end-of-headers (point-min)))
          (funcall pending-health nil)))
      (should (equal (list gemit-test--local-content nil) result))
      (should (= 2 (length records)))
      (should (equal (concat gemit-test--local-root
                             gemit-test--chat-completions-path)
                     (car (car records))))
      (should (equal "captured-model"
                     (plist-get
                      (gemit-test--json-request-body
                       (list :data (nth 2 (car records))))
                      :model)))
      (should (= 45 (aref (car timers) 0)))))))

(provide 'gemit-test)
;;; gemit-test.el ends here
