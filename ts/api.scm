(define-module (ts api)
  #:use-module (system foreign)
  #:use-module (system foreign-object)
  #:use-module (system foreign-library)
  #:use-module (oop goops)
  #:export (<ts-parser>
            ts-parser-language
            ts-parser-timeout
            get-ts-language-from-file
            ts-parser-parse-string
            ts-tree-root-node
            ts-tree-language
            ts-node-type))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts")
  (load-extension "libguile_ts" "init_ts_api"))

(define-class <ts-parser> (<%ts-parser>)
  (language #:allocation #:virtual
            #:slot-ref %tsp-language
            #:slot-set! %tsp-set-language!
            #:accessor ts-parser-language
            #:init-keyword #:language)
  (timeout #:allocation #:virtual
           #:slot-ref %tsp-timeout
           #:slot-set! %tsp-set-timeout!
           #:accessor ts-parser-timeout
           #:init-keyword #:timeout))

(define-method (initialize (obj <ts-parser>) initarg)
  (next-method obj (cons* #:%data (pointer-address (%tsp-new)) initarg)))

(define (get-ts-language-from-file lib name)
  (let ((o (foreign-library-function lib name #:return-type '*)))
    (if o (make <ts-language> #:%data (pointer-address (o)))
        #f)))
