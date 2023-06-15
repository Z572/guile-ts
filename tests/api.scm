(define-module (tests api)
  #:use-module (ts api)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64))

(test-group "ts-parser"
  (test-assert "make <ts-parser>" (make <ts-parser>))
  (test-assert "ts-parser-language is #f"
    (not (ts-parser-language (make <ts-parser>))))
  (test-error "<ts-parser> init language error value" (make <ts-parser>
                              #:language 20))
  (test-error "<ts-parser> init timeout error value" (make <ts-parser>
                              #:timeout 'a))
  (test-equal "ts-parser-timeout is 0"
    0 (ts-parser-timeout (make <ts-parser>)))
  (test-equal "ts-parser-time is 20"
    20 (ts-parser-timeout (make <ts-parser>
                            #:timeout 20)))
  (test-equal "set! ts-parser-time to 20"
    20 (let ((o (make <ts-parser>)))
         (set! (ts-parser-timeout o) 20)
         (ts-parser-timeout o))))
