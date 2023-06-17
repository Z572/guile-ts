(define-module (ts api)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-object)
  #:use-module (system foreign-library)
  #:use-module (oop goops)
  #:export (<ts-parser>
            <ts-range>
            ts-tree?
            ts-node?
            ts-parser-new
            ts-parser-reset!
            ts-parser-language
            ts-parser-included-ranges
            ts-parser-timeout
            get-ts-language-from-file
            ts-parser-parse-string
            ts-tree-root-node
            ts-tree-language
            ts-tree-delete
            ts-tree-copy
            ts-node-field-name-for-child
            ts-node-sexp
            ts-node-start-byte
            ts-node-start-point
            ts-node-end-byte
            ts-node-end-point
            ts-node-null?
            ts-node-named?
            ts-node-missing?
            ts-node-extra?
            ts-node-has-changes?
            ts-node-has-error?
            ts-node-parent
            ts-node-child
            ts-node-child-count
            ts-node-child-by-field-name
            ts-node-named-child-count
            ts-node-named-child
            ts-node-type
            ts-node-first-child-for-byte
            ts-node-first-named-child-for-byte
            ts-language-field-count
            ts-language-field-name-for-id
            ts-language-version
            ts-node-childs
            ts-node-named-childs
            ts-tree-cursor-new
            ts-tree-cursor-delete
            ts-tree-cursor-reset!
            ts-tree-cursor-current-node
            ts-tree-cursor-current-field-name
            ts-tree-cursor-copy))

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
           #:init-keyword #:timeout)
  (included-ranges #:allocation #:virtual
                   #:slot-ref %tsp-included-ranges
                   #:slot-set! %tsp-set-included-ranges!
                   #:accessor ts-parser-included-ranges
                   #:init-keyword #:include-ranges))

(define-method (equal? (node1 <ts-node>) (node2 <ts-node>))
  (%ts-node-eq? node1 node2))

(define-class <ts-range> (<%ts-range>)
  (start-point #:allocation #:virtual
               #:slot-ref %tsr-start-point
               #:slot-set! %tsr-set-start-point!
               #:accessor ts-range-start-point
               #:init-keyword #:start-point)
  (end-point #:allocation #:virtual
             #:slot-ref %tsr-end-point
             #:slot-set! %tsr-set-end-point!
             #:accessor ts-range-end-point
             #:init-keyword #:end-point)
  (start-byte #:allocation #:virtual
              #:slot-ref %tsr-start-byte
              #:slot-set! %tsr-set-start-byte!
              #:accessor ts-range-start-byte
              #:init-keyword #:start-byte)
  (end-byte #:allocation #:virtual
            #:slot-ref %tsr-end-byte
            #:slot-set! %tsr-set-end-byte!
            #:accessor ts-range-end-byte
            #:init-keyword #:end-byte))

(define-method (ts-node-sexp (node <ts-node>))
  (call-with-input-string (ts-node-string node) read))

(define (ts-tree? t)
  (is-a? t <ts-tree>))

(define-method (initialize (obj <ts-range>) initargs)
  (let ((data (get-keyword #:%data initargs #f)))
    (if data
        (next-method obj (cons* #:%data data initargs))
        (next-method obj (cons* #:%data (pointer-address (%make-tsr))
                                initargs)))))

(define-method (initialize (obj <ts-parser>) initarg)
  (next-method obj (cons* #:%data (pointer-address (%tsp-new)) initarg)))

(define* (ts-parser-new #:key
                        language
                        timeout
                        include-ranges)
  (apply make <ts-parser>
         `(,@(if language
                 `(#:language ,language)
                 '())
           ,@(if timeout
                 `(#:timeout ,timeout)
                 '())
           ,@(if include-ranges
                 `(#:include-ranges ,include-ranges)
                 '()))
         ))

(define (get-ts-language-from-file lib name)
  (let ((o (foreign-library-function lib name #:return-type '*)))
    (and o (%rf <ts-language> (o)))))

(define (ts-node? o)
  (is-a? o <ts-node>))
(define-method (ts-node-childs (node <ts-node>))
  (map (cut ts-node-child node <>)
       (iota (ts-node-child-count node))))

(define-method (ts-node-named-childs (node <ts-node>))
  (map (cut ts-node-named-child node <>)
       (iota (ts-node-named-child-count node))))
