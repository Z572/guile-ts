(define-module (tests gc)
  #:use-module (ts)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64))

(test-group "gc test"
  (let* ((source "[1,null]")
         (parser (make <ts-parser>
                   #:language
                   (get-ts-language-from-file
                    "tree-sitter-json"
                    "tree_sitter_json")))
         (tree (ts-parser-parse-string parser #f source))
         (root (ts-tree-root-node tree))
         (n 100000))

    (test-assert (string-append "gc " (number->string n))
      (begin
        (do ((i 0 (+ i 1)))
            ((> i n))
          (ts-node-next-sibling
           (ts-node-child
            root
            (- (ts-node-child-count root #t) 1)
            #t)
           #t))
        (ts-node-sexp root)))))
