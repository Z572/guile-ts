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

(define <ts-language>
  (make-foreign-object-type
   '<ts-language> '(%data)))

(eval-when (expand load eval)
  (unless (getenv "GUILE_TS_CROSS_COMPILING")
    (load-extension "libguile_ts" "init_ts_language")))

(define (get-ts-language-from-file lib name)
  (parameterize ((ltdl-library-path
                  (append
                   (parse-path (getenv "TREE_SITTER_GRAMMAR_PATH"))
                   (ltdl-library-path))))
    (let ((o (foreign-library-function lib name #:return-type '*)))
      (and o (find-or-create-fobject <ts-language> (o))))))
