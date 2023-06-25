(define-module (ts language)
  #:use-module (ts init)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (get-ts-language-from-file
            ts-language-field-count
            ts-language-symbol-count
            ts-language-field-name-for-id
            ts-language-version))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_language"))

(define (get-ts-language-from-file lib name)
  (let ((o (foreign-library-function lib name #:return-type '*)))
    (and o (%rf <ts-language> (o)))))
