(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-url*
  "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.0-flash:generateContent")

(defun generate (prompt)
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post *gemini-api-url*
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           (candidates-pair (assoc :CANDIDATES decoded-response))
           (candidates (cdr candidates-pair))
           (candidate (first candidates))
           (content-pair (assoc :CONTENT candidate))
           (content (cdr content-pair))
           (parts-pair (assoc :PARTS content))
           (parts (cdr parts-pair))
           (first-part (first parts))
           (text-pair (assoc :TEXT first-part))
           (text (cdr text-pair)))
       text)))

;; (gemini:generate "In one sentence, explain how AI works to a child.")
;; (gemini:generate "Write a short, four-line poem about coding in Python.")