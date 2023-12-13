(define-module (ts util)
  #:use-module (ts init)
  #:use-module (ts language)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors gnu)
  #:export (<ts-range>
            substring-utf8))

(define* (substring-utf8 str start #:optional end)
  "Guile's substring is no support utf-8, but tree-sitter returns location is
utf-8 base.
e.g.
(substring \"conf='…'\\n\" 5 10) will error.
(substring-utf8 \"conf='…'\\n\" 5 10) is normal.
"
  (let ((bv (string->bytevector str "utf-8")))
    (bytevector->string
     (if end
         (bytevector-slice bv start (- end start))
         (bytevector-slice bv start))
     "utf-8")))

(define <%ts-range>
  (make-foreign-object-type
   '<%ts-range> '(%data)))

(eval-when (expand load eval)
  (if (getenv "GUILE_TS_CROSS_COMPILING")
      (for-each (lambda (x)
                  (module-define! (current-module) x identity))
                '(%tsr-start-point
                  %tsr-end-point
                  %tsr-end-byte
                  %tsr-start-byte
                  %tsr-set-start-point!
                  %tsr-set-end-point!
                  %tsr-set-start-byte!
                  %tsr-set-end-byte!))
      (load-extension "libguile_ts" "init_ts_util")))

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
