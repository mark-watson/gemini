;;;; package.lisp

(defpackage #:gemini
  (:use #:cl #:uiop #:json #:dexador)
  (:shadow "PARAMETER-ERROR")
  (:export #:research))
