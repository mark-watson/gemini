(defpackage #:gemini-tests
  (:use #:cl #:fiveam #:gemini)
  (:import-from #:dexador #:post) ; Import to be able to redefine it
  (:import-from #:flexi-streams #:make-in-memory-input-stream #:string-to-octets)
  (:import-from #:alexandria #:plist-hash-table #:starts-with-subseq))

(in-package #:gemini-tests)

;;; Global test state
(def-suite gemini-suite :description "Test suite for Gemini API client.")
(in-suite gemini-suite)

;; Set a dummy API key for all tests
(defvar gemini::*google-api-key* "dummy-test-key")

;; Mocking infrastructure for dexador:post
(defvar *mock-dex-post-handler* nil "Holds the current mock handler for dex:post.")
(defvar *original-dex-post* #'dexador:post "To store the original dexador:post function.")

;; Redefine dexador:post for testing purposes.
;; This definition will be active when the tests are loaded and run.
(defun dexador:post (url &key headers content want-stream)
  (if *mock-dex-post-handler*
      (funcall *mock-dex-post-handler* url :headers headers :content content :want-stream want-stream)
      (error "dexador:post call not mocked! URL: ~A. Current handler: ~S" url *mock-dex-post-handler*)))

;;; Tests

(test generate-success
  (let ((*mock-dex-post-handler*
          (lambda (url &key headers content want-stream)
            (declare (ignore headers content want-stream))
            (is (string-equal url "https://generativelanguage.googleapis.com/v1beta/models/test-model:generateContent"))
            "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Mocked response text\"}]}}]}")))
    (is (string= (gemini:generate "test-model" "A prompt") "Mocked response text"))))

(test count-tokens-success
  (let ((*mock-dex-post-handler*
          (lambda (url &key headers content want-stream)
            (declare (ignore headers content want-stream))
            (is (string-equal url "https://generativelanguage.googleapis.com/v1beta/models/test-model:countTokens"))
            "{\"totalTokens\": 123}")))
    (is (= (gemini:count-tokens "test-model" "A prompt") 123))))

(test send-chat-message-success
  (let ((*mock-dex-post-handler*
          (lambda (url &key headers content want-stream)
            (declare (ignore headers want-stream))
            (is (string-equal url "https://generativelanguage.googleapis.com/v1beta/models/chat-model:generateContent"))
            ;; Check that the content has the correct structure for chat
            (let ((json-payload (cl-json:decode-json-from-string content)))
              (is (equalp (gethash "role" (first (cdr (assoc :CONTENTS json-payload)))) "user"))
              (is (equalp (gethash "text" (first (cdr (assoc :PARTS (first (cdr (assoc :CONTENTS json-payload))))))) "Hello")))
            "{\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Mocked chat response\"}]}}]}")))
    (let ((messages (list (alexandria:plist-hash-table '("role" "user" "text" "Hello") :test 'equal))))
      (is (string= (gemini:send-chat-message "chat-model" messages) "Mocked chat response")))))

(test generate-streaming-success
  (let* ((accumulated-text (make-string-output-stream))
         (*mock-dex-post-handler*
           (lambda (url &key headers content want-stream)
             (declare (ignore headers content))
             (is (string-equal url "https://generativelanguage.googleapis.com/v1beta/models/stream-model:streamGenerateContent"))
             (is want-stream) ; Ensure streaming is requested
             ;; Simulate SSE stream
             (let* ((sse-data1 "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Stream chunk 1 \"}]}}]}\n\n")
                    (sse-data2 "data: {\"candidates\":[{\"content\":{\"parts\":[{\"text\":\"Stream chunk 2\"}]}}]}\n\n")
                    (full-sse-response (concatenate 'string sse-data1 sse-data2)))
               (flexi-streams:make-in-memory-input-stream (flexi-streams:string-to-octets full-sse-response :external-format :utf-8))))))
    (let ((result (gemini:generate-streaming "stream-model" "A streaming prompt"
                                            (lambda (text-chunk)
                                              (write-string text-chunk accumulated-text)))))
      (is-true result) ; Function should return T on completion
      (is (string= (get-output-stream-string accumulated-text) "Stream chunk 1 Stream chunk 2")))))

;; To run from SLIME:
;; (asdf:test-system :gemini)
;; Or, load the file and run:
;; (fiveam:run! 'gemini-suite)
;; (fiveam:run! 'generate-success)
;; (fiveam:run! 'count-tokens-success)
;; (fiveam:run! 'send-chat-message-success)
;; (fiveam:run! 'generate-streaming-success)

;; Restore original dexador:post if tests are reloaded interactively (optional, good practice)
;; This is tricky because this file will be reloaded by ASDF. A more robust solution
;; would involve a test runner that sets up and tears down mocks.
;; For this task, we assume ASDF handles loading once per test run.
;; To manually restore if needed:
;; (setf dexador:post *original-dex-post*)
