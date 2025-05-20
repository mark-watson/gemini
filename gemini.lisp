(in-package #:gemini)

(defvar *google-api-key* (uiop:getenv "GOOGLE_API_KEY"))
(defvar
  *gemini-api-base-url*
  "https://generativelanguage.googleapis.com/v1beta/models/")

(defun generate (model-id prompt)
  "Generates text from a given prompt using the specified model.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
PROMPT: The text prompt to generate content from.
Returns the generated text as a string."
  (let* ((payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
           (data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
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

(defun generate-streaming (model-id prompt callback)
  "Generates text from a prompt using the specified model, streaming the response.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
PROMPT: The text prompt to generate content from.
CALLBACK: A function that will be called with each chunk of generated text as it arrives.
Returns T to indicate completion, as text is handled by the callback."
  (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":streamGenerateContent"))
         (payload (make-hash-table :test 'equal)))
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (stream (dex:post api-url
                             :headers `(("Content-Type" . "application/json")
                                        ("x-goog-api-key" . ,*google-api-key*))
                             :content data
                             :want-stream t)))
      (unwind-protect
           (loop for line = (read-line stream nil nil)
                 while line
                 do
                    (when (alexandria:starts-with-subseq "data: " line)
                      (let* ((json-string (subseq line 6)) ;; Skip "data: "
                             (decoded-chunk (ignore-errors (cl-json:decode-json-from-string json-string))))
                        (when (and decoded-chunk (listp decoded-chunk)) ; Check if it's a valid alist
                          (let* ((candidates-pair (assoc :CANDIDATES decoded-chunk))
                                 (candidates (and candidates-pair (cdr candidates-pair)))
                                 (candidate (and candidates (first candidates)))
                                 (content-pair (and candidate (assoc :CONTENT candidate)))
                                 (content (and content-pair (cdr content-pair)))
                                 (parts-pair (and content (assoc :PARTS content)))
                                 (parts (and parts-pair (cdr parts-pair)))
                                 (first-part (and parts (first parts)))
                                 (text-pair (and first-part (assoc :TEXT first-part)))
                                 (text (and text-pair (cdr text-pair))))
                            (when text
                              (funcall callback text)))))))
        (close stream))
      t))) ; Indicate completion

;; (gemini:generate "gemini-1.5-flash-latest" "In one sentence, explain how AI works to a child.")
;; (gemini:generate "gemini-1.5-flash-latest" "Write a short, four-line poem about coding in Python.")

(defun count-tokens (model-id prompt)
  "Counts the number of tokens for a given prompt and model.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
PROMPT: The text prompt to count tokens for.
Returns the total token count as an integer."
  (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":countTokens"))
         (payload (make-hash-table :test 'equal)))
    ;; Construct payload similar to generate function
    (setf (gethash "contents" payload)
          (list (let ((contents (make-hash-table :test 'equal)))
                  (setf (gethash "parts" contents)
                        (list (let ((part (make-hash-table :test 'equal)))
                                (setf (gethash "text" part) prompt)
                                part)))
                  contents)))
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; cl-json by default uses :UPCASE for keys, so :TOTAL-TOKENS should be correct
           (total-tokens-pair (assoc :TOTAL-TOKENS decoded-response)))
      (if total-tokens-pair
          (cdr total-tokens-pair)
          (error "Could not retrieve token count from API response: ~S" decoded-response)))))

(defun send-chat-message (model-id messages)
  "Sends a series of messages as a chat history and gets a response from the specified model.
MODEL-ID: The ID of the model to use (e.g., \"gemini-1.5-pro-latest\", \"gemini-1.5-flash-latest\").
MESSAGES: A list of hash-tables, where each hash-table must have a \"role\" (string: \"user\" or \"model\")
          and a \"text\" (string: the message content).
          Example: (list (alexandria:plist-hash-table '(\"role\" \"user\" \"text\" \"Hello!\"))
                         (alexandria:plist-hash-table '(\"role\" \"model\" \"text\" \"Hi there!\")))
Returns the model's response text as a string."
  (let* ((api-url (concatenate 'string *gemini-api-base-url* model-id ":generateContent"))
         (payload (make-hash-table :test 'equal))
         (api-contents (mapcar (lambda (msg)
                                 (let ((content-ht (make-hash-table :test 'equal)))
                                   (setf (gethash "role" content-ht) (gethash "role" msg))
                                   (setf (gethash "parts" content-ht)
                                         (list (let ((part-ht (make-hash-table :test 'equal)))
                                                 (setf (gethash "text" part-ht) (gethash "text" msg))
                                                 part-ht)))
                                   content-ht))
                               messages)))
    (setf (gethash "contents" payload) api-contents)
    ;; The rest is similar to the 'generate' function:
    ;; encode payload, make dex:post request, decode response, extract text
    (let* ((data (cl-json:encode-json-to-string payload))
           (response (dex:post api-url
                               :headers `(("Content-Type" . "application/json")
                                          ("x-goog-api-key" . ,*google-api-key*))
                               :content data))
           (response-string (if (stringp response)
                                response
                                (flex:octets-to-string response :external-format :utf-8)))
           (decoded-response (cl-json:decode-json-from-string response-string))
           ;; Standard response extraction logic from 'generate'
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