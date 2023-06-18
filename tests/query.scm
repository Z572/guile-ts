(define-module (tests query)
  #:use-module (ts api)
  #:use-module (ts query)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-71))
(test-group "ts-query"
  (let* ((ts-json
          (get-ts-language-from-file
           (string-append (getenv "abs_top_builddir")
                          "/tests/tree-sitter-json.so")
           "tree_sitter_json"))
         (source "[1]"))
    (test-assert "ts-query-new"
      (ts-query-new ts-json "(document)"))
    (let ((query offset type (ts-query-new ts-json " (")))
      (test-assert "ts-query-new: fail"
        (and (not query)
             (= offset 2)
             (= type TSQueryErrorSyntax))))
    (let ((qc (ts-query-cursor-new)))
      (ts-query-cursor-delete qc)
      (test-error "ts-query-cursor-delete: delete twice."
                  #t (ts-query-cursor-delete qc)))
    (let* ((qc (ts-query-cursor-new))
           (query (ts-query-new ts-json "(document)"))
           (parser (make <ts-parser> #:language ts-json))
           (tree (ts-parser-parse-string parser #f source))
           (root-node (ts-tree-root-node tree)))
      (ts-query-cursor-exec qc query root-node)
      (test-assert "ts-query-cursor-exec"
        (ts-query-pattern-rooted? query 0))
      (ts-query-cursor-delete qc)
      (ts-tree-delete tree))))
