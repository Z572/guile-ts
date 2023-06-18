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

  (let* ((parser (make <ts-parser> #:language json-language)))
    (test-error "ts-parser-parse-string: out of lenght"
                'out-of-range
                (ts-parser-parse-string parser #f "[1,null]" 200))
    (test-error "ts-parser-parse-string: wrong-type-arg"
                'wrong-type-arg
                (ts-parser-parse-string parser #f "[1,null]" #())))

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
    (test-error "ts-node-child: named out of range 1"
                'out-of-range
                (ts-node-child root 30 #t))
    (let ((node (ts-node-child
                 (ts-node-child root 0) 0 #t) ))
      (test-equal "substring/read-only"
        "1"
        (substring/read-only
         source
         (ts-node-start-byte node)
         (ts-node-end-byte node))))
    (test-equal "ts-node-start-byte"
      (string-length source) (ts-node-end-byte root))

    (test-equal "ts-node-first-child-for-byte: named"
      (ts-node-child root 0)
      (ts-node-first-child-for-byte root 0 #t))
    (test-assert "ts-node-first-child-for-byte: > length"
      (not (ts-node-first-child-for-byte root (string-length source) #t)))
    (test-error
     "child out of range"
     'out-of-range
     (ts-node-child root (1+ (ts-node-child-count root))))
    (test-equal "ts-node-type"
      "document" (ts-node-type root))
    (test-equal "ts-node-prev-sibling"
      #f (ts-node-prev-sibling
          (ts-node-child root 0)))
    (test-equal "ts-node-next-sibling"
      #f (ts-node-next-sibling
          (ts-node-child root (- (ts-node-child-count root) 1))))

    (test-equal "ts-node-prev-sibling: named"
      #f (ts-node-prev-sibling
          (ts-node-child root 0 #t)
          1
          #t))
    (test-equal "ts-node-next-sibling: named"
      #f
      (ts-node-next-sibling
       (ts-node-child
        root
        (- (ts-node-named-child-count root) 1) #t)
       #t))

    (test-equal "ts-node-descendant-for-byte-range"
      (ts-node-child root 0)
      (ts-node-descendant-for-byte-range (ts-node-child root 0) 0 1))

    (test-equal "ts-node-descendant-for-byte-range: named"
      (ts-node-child root 0 #t)
      (ts-node-descendant-for-byte-range (ts-node-child root 0) 0 1 #t))

    (ts-tree-delete tree))
  (let* ((source "[1")
         (parser (make <ts-parser>
                   #:language json-language))
         (tree (ts-parser-parse-string parser #f source))
         (root (ts-tree-root-node tree)))
    (test-assert "ts-node-error"
      (ts-node-has-error? root))
    (test-assert "ts-node-sexp: error string"
      (ts-node-sexp root))
    (ts-tree-delete tree)))
