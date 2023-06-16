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
  (test-equal "ts-parser-time is 20"
    20 (ts-parser-timeout (make <ts-parser>
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
  (test-assert "parse string"
    (let ((parser (make <ts-parser> #:language json-language)))
      (ts-tree? (ts-parser-parse-string parser #f "[1,null]"))))
  (let* ((source "[1,null]")
         (parser (make <ts-parser> #:language json-language))
         (root (ts-tree-root-node
                (ts-parser-parse-string parser #f source))))
    (test-equal "root-node's parent"
      #f
      (ts-node-parent
       (ts-tree-root-node
        (ts-parser-parse-string parser #f "[1,null]"))))
    (test-error
     "child out of range"
     'wrong-type-arg
     (ts-node-child root (1+ (ts-node-child-count root))))
    (test-equal "ts-node-type"
      "document" (ts-node-type root))
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
        (- (ts-node-named-child-count root) 1))))))
