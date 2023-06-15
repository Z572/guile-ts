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
  (test-assert "parse string"
    (let ((parser (make <ts-parser> #:language json-language)))
      (ts-tree? (ts-parser-parse-string parser #f "[1,null]"))))
  (test-equal "root-node's parent"
    #f
    (let ((parser (make <ts-parser> #:language json-language)))
      (ts-node-parent
       (ts-tree-root-node
        (ts-parser-parse-string parser #f "[1,null]")))))
  (test-error
   "child out of range"
   'wrong-type-arg
   (let* ((parser (make <ts-parser> #:language json-language))
          (root (ts-tree-root-node
                 (ts-parser-parse-string parser #f "[1,null]"))))
     (ts-node-child root (1+ (ts-node-child-count root))))))
