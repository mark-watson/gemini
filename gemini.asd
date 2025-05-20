;;;; gemini.asd

(asdf:defsystem #:gemini
  :description "Library for using the perplexity search+LLM APIs"
  :author "Mark Watson"
  :license "Apache 2"
  :depends-on (#:uiop #:cl-json #:dexador #:alexandria #:fiveam) ; Added fiveam
  :components ((:file "package")
               (:file "gemini")
               (:file "tests/test-gemini")) ; Added test file
  :perform (asdf:test-op (op c) (fiveam:run! 'gemini-tests::gemini-suite))) ; Added test operation
