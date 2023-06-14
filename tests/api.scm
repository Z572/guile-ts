(define-module (tests api)
  #:use-module (ts api)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64))

(test-group "server"
  (test-assert "make <ts-parser>" (make <ts-parser>))
  (test-assert "ts-language is #f"
    (not (ts-parser-language (make <ts-parser>)))))
