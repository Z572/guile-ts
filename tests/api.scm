(define-module (tests api)
  #:use-module (ts api)
  #:use-module (oop goops)
  #:use-module (srfi srfi-64))

(test-group "ts-parser"
  (test-assert "make <ts-parser>" (make <ts-parser>))
  (test-assert "ts-parser-language is #f"
    (not (ts-parser-language (make <ts-parser>))))
  (test-error "<ts-parser> init language error value" #t
              (make <ts-parser>
                #:language 20))
  (test-error "<ts-parser> init timeout error value" #t
              (make <ts-parser>
                #:timeout 'a))
  (test-equal "ts-parser-parse-string without langeuage"
    #f (ts-parser-parse-string (make <ts-parser>) #f "[]"))
  (test-equal "ts-parser-timeout is 0"
    0 (ts-parser-timeout (make <ts-parser>)))
  (test-equal "make <ts-parser>: timeout is 20"
    20 (ts-parser-timeout (make <ts-parser>
                            #:timeout 20)))
  (test-equal "ts-parser-new: timeout is 20"
    20 (ts-parser-timeout (ts-parser-new
                           #:timeout 20)))
  (test-equal "set! ts-parser-time to 20"
    20 (let ((o (make <ts-parser>)))
         (set! (ts-parser-timeout o) 20)
         (ts-parser-timeout o)))
  (test-error "error when get noexits lib"
              #t
              (get-ts-language-from-file "noexits-lib" "noexits-func"))
  (define json-language #f)
  (test-assert "load language from so"
    (let ((o (get-ts-language-from-file
              (string-append (getenv "abs_top_builddir")
                             "/tests/tree-sitter-json.so")
              "tree_sitter_json")))
      (set! json-language o)
      o))
  (test-equal "set language"
    json-language
    (ts-parser-language
     (make <ts-parser> #:language json-language)))
  (test-equal "language version"
    14 (ts-language-version json-language))
  (test-equal "ts-language-field-count"
    2 (ts-language-field-count json-language))
  (test-equal "ts-language-field-name-for-id"
    "key" (ts-language-field-name-for-id json-language 1))
  (test-error "ts-language-field-name-for-id: out of value"
              #t
              (ts-language-field-name-for-id json-language 30))

  (let* ((parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f "[1,null]")))
    (test-assert "parse string"
      (ts-tree? tree))
    (when (ts-tree? tree)
      (ts-tree-delete tree)))

  (let* ((source "[1,null]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f source))
         (root (ts-tree-root-node tree)))
    (test-equal "root-node's parent"
      #f
      (ts-node-parent
       (ts-tree-root-node
        (ts-parser-parse-string parser #f "[1,null]"))))

    (test-equal "ts-node-sexp"
      '(document (array (number) (null)))
      (ts-node-sexp root))
    (test-assert "ts-tree-copy"
      (not (equal? tree (ts-tree-copy tree))))
    (test-error "ts-node-named-child: out of range 1"
                'out-of-range
                (ts-node-named-child root 30))
    (let ((node (ts-node-named-child
                 (ts-node-child root 0) 0) ))
      (test-equal "substring/read-only"
        "1"
        (substring/read-only
         source
         (ts-node-start-byte node)
         (ts-node-end-byte node))))
    (test-equal "ts-node-start-byte"
      (string-length source) (ts-node-end-byte root))

    (test-equal "ts-node-first-named-child-for-byte"
      (ts-node-child root 0)
      (ts-node-first-named-child-for-byte root 0))
    (test-assert "ts-node-first-named-child-for-byte: > length"
      (not (ts-node-first-named-child-for-byte root (string-length source))))
    (test-error
     "child out of range"
     'out-of-range
     (ts-node-child root (1+ (ts-node-child-count root))))
    (test-equal "ts-node-type"
      'document (ts-node-type root))
    (test-equal "ts-node-prev-sibling"
      #f (ts-node-prev-sibling
          (ts-node-child root 0)))
    (test-equal "ts-node-next-sibling"
      #f (ts-node-next-sibling
          (ts-node-child root (- (ts-node-child-count root) 1))))

    (test-equal "ts-node-prev-named-sibling"
      #f (ts-node-prev-named-sibling
          (ts-node-named-child root 0)))
    (test-equal "ts-node-next-named-sibling"
      #f
      (ts-node-next-named-sibling
       (ts-node-named-child
        root
        (- (ts-node-named-child-count root) 1))))
    (ts-tree-delete tree)))
