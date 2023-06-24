(define-module (ts util)
  #:use-module (ts init)
  #:use-module (ts language)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:export (<ts-range>))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_util"))

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

(define-method (equal? (range1 <ts-range>) (range2 <ts-range>))
  (and (equal? (ts-range-start-point range1) (ts-range-start-point range2))
       (equal? (ts-range-end-point range1) (ts-range-end-point range2))
       (equal? (ts-range-start-byte range1) (ts-range-start-byte range2))
       (equal? (ts-range-end-byte range1) (ts-range-end-byte range2))))

(define-method (initialize (obj <ts-range>) initargs)
  (let ((data (get-keyword #:%data initargs #f)))
    (if data
        (next-method obj (cons* #:%data data initargs))
        (next-method obj (cons* #:%data (pointer-address (%make-tsr))
                                initargs)))))
