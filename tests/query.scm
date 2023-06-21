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
    (let ((q (ts-query-new ts-json "(document)")))
      (test-assert "ts-query-new" q)
      (and q (ts-query-delete q) ))
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
      (ts-tree-delete tree)
      (ts-query-delete query))
    (let* ((query (ts-query-new
                   ts-json
                   "(document (array (number) @num (number) @num2))")))
      (test-equal "ts-query-pattern-count"
        1 (ts-query-pattern-count query))
      (test-equal "ts-query-capture-count"
        2 (ts-query-capture-count query))
      (test-equal "ts-query-string-count"
        0 (ts-query-string-count query))
      (ts-query-delete query))



    (let* ((query (ts-query-new
                   ts-json
                   "(#a)")))

      (test-equal "ts-query-string-count: 1"
        ;; #a
        1(ts-query-string-count query))
      (ts-query-delete query))

    (let* ((query (ts-query-new
                   ts-json
                   "(#a \"h\")")))

      (test-equal "ts-query-string-count: 2"
        ;; #a "h"
        2 (ts-query-string-count query))
      (ts-query-delete query))
    (let* ((qc (ts-query-cursor-new))
           (query (ts-query-new ts-json "(document (array (number) @b . (number) @bc))"))
           (parser (make <ts-parser> #:language ts-json))
           (tree (ts-parser-parse-string parser #f "[1,2,3]"))
           (root-node (ts-tree-root-node tree)))
      (ts-query-cursor-exec qc query root-node)
      (test-equal "ts-query-capture-name-for-id: 1"
        "b" (ts-query-capture-name-for-id query 0))
      (test-equal "ts-query-capture-name-for-id: 2"
        "bc" (ts-query-capture-name-for-id query 1))
      (test-error "ts-query-capture-name-for-id: out of range"
                  'out-of-range
                  (ts-query-capture-name-for-id query 2))
      (ts-query-cursor-delete qc)
      (ts-tree-delete tree)
      (ts-query-delete query))))
