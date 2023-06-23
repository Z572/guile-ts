(define-module (ts api)
  #:use-module (ts init)
  #:use-module (ts language)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (<ts-parser>
            <ts-range>
            ts-parser-included-ranges
            ts-parser-language
            ts-parser-new
            ts-parser-parse-string
            ts-parser-reset!
            ts-parser-timeout
            ts-parser-logger
            ts-parser-print-dot-graphs))

(eval-when (expand load eval)
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
                   #:init-keyword #:include-ranges)
  (logger #:allocation #:virtual
          #:slot-ref %tsp-logger
          #:slot-set! %tsp-set-logger!
          #:accessor ts-parser-logger))

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
                 '()))))

(define (ts-parser-print-dot-graphs parser fd-or-port)
  (let ((fd (if (port? fd-or-port)
                (port->fdes fd-or-port)
                fd-or-port)))
    (%ts-parser-print-dot-graphs parser fd)))
