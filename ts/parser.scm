(define-module (ts parser)
  #:use-module (ts init)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (<ts-parser>
            ts-parser-included-ranges
            ts-parser-language
            ts-parser-new
            ts-parser-parse-string
            ts-parser-parse
            ts-parser-reset!
            ts-parser-timeout
            ts-parser-logger
            ts-parser-print-dot-graphs))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_parser"))

(define (tsp-delete! o)
  (let ((%data (slot-ref o '%data)))
    (%tsp-delete! (make-pointer %data))))

(define <%ts-parser>
  (make-foreign-object-type
   '<%ts-parser> '(%data)
   #:finalizer tsp-delete!))
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
                   #:init-keyword #:include-ranges)
  (logger #:allocation #:virtual
          #:slot-ref %tsp-logger
          #:slot-set! %tsp-set-logger!
          #:accessor ts-parser-logger)
  #:finalizer tsp-delete!)

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
                 '()))))

(define ts-parser-parse-string
  (case-lambda
    "Returns a <ts-tree> object, @var{old-tree} need @var{<ts-tree>} or
@var{#f}, @var{string} is expect you source code.
"
    ((parser string)
     (%ts-parser-parse-string parser #f string))
    ((parser old-tree string)
     (%ts-parser-parse-string parser old-tree string))
    ((parser old-tree string length)
     (%ts-parser-parse-string parser old-tree string length))))

(define ts-parser-parse
  (case-lambda
    ((parser object proc)
     (%ts-parser-parse parser #f object proc))
    ((parser old-tree object proc)
     (%ts-parser-parse parser old-tree object proc))))

(define (ts-parser-print-dot-graphs parser fd-or-port)
  (let ((fd (if (port? fd-or-port)
                (port->fdes fd-or-port)
                fd-or-port)))
    (%ts-parser-print-dot-graphs parser fd)))
