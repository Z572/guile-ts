(define-module (ts init)
  #:use-module (oop goops)
  #:use-module (system foreign-object)
  #:use-module ((system foreign) #:prefix ffi:)
  #:export (find-or-create-fobject))

(define %foreign-objects (make-weak-value-hash-table 3000))

(define (find-fobject pointer)
  (hash-ref %foreign-objects pointer #f))

(define (find-or-create-fobject type pointer)
  (let ((exists? (find-fobject pointer)))
    (or exists?
        (let ((new-obj (make type #:%data (ffi:pointer-address pointer))))
          (hash-set! %foreign-objects pointer new-obj)
          new-obj))))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts"))
