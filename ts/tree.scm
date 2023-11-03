(define-module (ts tree)
  #:use-module (ts init)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (system foreign-object)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:export (ts-node-child
            ts-node-child-by-field-name
            ts-node-child-count
            ts-node-childs
            ts-node-descendant-for-byte-range
            ts-node-end-byte
            ts-node-end-point
            ts-node-extra?
            ts-node-field-name-for-child
            ts-node-first-child-for-byte
            ts-node-has-changes?
            ts-node-has-error?
            ts-node-missing?
            ts-node-prev-sibling
            ts-node-next-sibling
            ts-node-named?
            ts-node-null?
            ts-node-parent
            ts-node-sexp
            ts-node-start-byte
            ts-node-start-point
            ts-node-type
            ts-node?
            ts-tree-copy
            ts-tree-cursor-copy
            ts-tree-cursor-current-field-name
            ts-tree-cursor-current-node
            ts-tree-cursor-goto-parent
            ts-tree-cursor-goto-first-child
            ts-tree-cursor-goto-first-child-for-byte
            ts-tree-cursor-goto-first-child-for-point
            ts-tree-cursor-goto-next-sibling
            ts-tree-cursor-new
            ts-tree-cursor-reset!
            ts-tree-language
            ts-tree-root-node
            ts-tree?
            ts-tree-edit
            ts-tree-get-changed-ranges))

(define (%tcursor-finalizer o)
  (let ((%data (slot-ref o '%data)))
    (%tcursor_finalizer (make-pointer %data))))

(define (%ts-tree-delete! o)
  (let ((%data (slot-ref o '%data)))
    (%ts_tree_delete (make-pointer %data))))

(define (%node-finalizer o)
  (let ((%data (slot-ref o '%data)))
    (%node_finalizer (make-pointer %data))))

(define <ts-node>
  (make-foreign-object-type
   '<ts-node> '(%data)
   #:finalizer %node-finalizer))
(define <ts-tree>
  (make-foreign-object-type
   '<ts-tree> '(%data)
   #:finalizer %ts-tree-delete!))
(define <ts-tree-cursor>
  (make-foreign-object-type
   '<ts-tree-cursor> '(%data)
   #:finalizer %tcursor-finalizer))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_tree"))

(define-method (equal? (node1 <ts-node>) (node2 <ts-node>))
  (%ts-node-eq? node1 node2))

(define-method (write (node <ts-node>) port)
  (format port "#<~a ~s in ~a-~a ~x>"
          (class-name (class-of node))
          (ts-node-type node)
          (ts-node-start-byte node)
          (ts-node-end-byte node)
          (object-address node)))

(define* (ts-tree-root-node tree #:optional offset-bytes (offset-point #f))
  (if (or offset-bytes offset-point)
      (apply %ts-tree-root-node-with-offset tree
             (if offset-point
                 (list offset-bytes offset-point)

                 (if (pair? offset-bytes)
                     (list 0 offset-bytes)
                     (list offset-bytes (cons 0 0)))))
      (%ts-tree-root-node tree)))

(define* (ts-tree-edit tree start old-end new-end
                       start-point
                       old-end-point
                       new-end-point)
  (%ts-tree-edit
   tree
   (list start old-end new-end
         start-point old-end-point new-end-point)))

;; node
(define-method (ts-node-sexp (node <ts-node>))
  (call-with-input-string (ts-node-string node) read))

(define (ts-node? o)
  (is-a? o <ts-node>))

(define (ts-tree? t)
  (is-a? t <ts-tree>))

(define* (ts-node-childs node #:optional (named? #f))
  (map (cut ts-node-child node <> named?)
       (iota (ts-node-child-count node named?))))
