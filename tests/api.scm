(define-module (tests api)
  #:use-module (ts)
  #:use-module (oop goops)
  #:use-module (ice-9 textual-ports)
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
              "tree-sitter-json"
              "tree_sitter_json")))
      (set! json-language o)
      o))
  (test-equal "set language"
    json-language
    (ts-parser-language
     (make <ts-parser> #:language json-language)))
  (test-equal "language version"
    14 (ts-language-version json-language))
  (test-equal "ts-language-symbol-count"
    26 (ts-language-symbol-count json-language))
  (test-equal "ts-language-field-count"
    2 (ts-language-field-count json-language))
  (test-equal "ts-language-field-name-for-id"
    "key" (ts-language-field-name-for-id json-language 1))
  (test-error "ts-language-field-name-for-id: out of value"
              #t
              (ts-language-field-name-for-id json-language 30))


  (test-assert "ts-parser-parse-string: 2 arg"
    (let* ((parser (make <ts-parser> #:language json-language))
           (tree (ts-parser-parse-string parser "[1,null]")))
      (ts-tree? tree)))
  (test-assert "ts-parser-parse-string: 3 arg"
    (let* ((parser (make <ts-parser> #:language json-language))
           (tree (ts-parser-parse-string parser #f "[1,null]")))
      (ts-tree? tree)))
  (test-assert "ts-parser-parse-string: 4 arg"
    (let* ((parser (make <ts-parser> #:language json-language))
           (tree (ts-parser-parse-string parser #f "[1,null]" 3)))
      (ts-tree? tree)))

  (let* ((parser (make <ts-parser> #:language json-language)))
    (test-error "ts-parser-parse-string: out of lenght"
                'out-of-range
                (ts-parser-parse-string parser #f "[1,null]" 200))
    (test-error "ts-parser-parse-string: wrong-type-arg"
                'wrong-type-arg
                (ts-parser-parse-string parser #f "[1,null]" #())))

  (let* ((source "[11]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f source)))
    (test-equal "ts-tree-root-node: offset, byte"
      200 (ts-node-start-byte
           (ts-tree-root-node tree 200 (cons 0 200))))
    (test-equal "ts-tree-root-node: offset, point"
      (cons 0 200) (ts-node-start-point
                    (ts-tree-root-node tree 200 (cons 0 200))))

    (test-equal "ts-tree-root-node: offset, point only"
      (cons 0 200) (ts-node-start-point
                    (ts-tree-root-node tree (cons 0 200))))
    (test-equal "ts-tree-root-node: offset, byte only"
      200 (ts-node-start-byte
           (ts-tree-root-node tree 200))))
  (let* ((old-source "[11]")
         (new-source "[20,11]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f old-source)))
    (test-equal "ts-tree-edit"
      '(document (array (number) (number)))
      (begin (ts-tree-edit tree 0 (string-length old-source) (string-length new-source)
                           (cons 0 0)
                           (cons 0 (string-length old-source))
                           (cons 0 (string-length new-source)))
             (ts-node-sexp
              (ts-tree-root-node
               (ts-parser-parse-string parser tree new-source))))))
  (let* ((old-source "[11]")
         (new-source "[20,11]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f old-source)))
    (test-assert "ts-node-has-changes?"
      (begin (ts-tree-edit tree 0
                           (string-length old-source)
                           (string-length new-source)
                           (cons 0 0)
                           (cons 0 (string-length old-source))
                           (cons 0 (string-length new-source)))

             (ts-node-has-changes?
              (ts-node-child (ts-node-child
                              (ts-tree-root-node tree) 0) 0 #t)))))
  (let* ((old-source "[11]")
         (new-source "[21,11]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f old-source)))
    (test-equal "ts-tree-get-changed-ranges"
      (list (make <ts-range>
              #:start-byte 1
              #:end-byte 4
              #:start-point '(0 . 1)
              #:end-point '(0 . 4)))
      (begin
        (ts-tree-edit tree 1 1 4
                      (cons 0 1)
                      (cons 0 1)
                      (cons 0 4))
        (ts-tree-get-changed-ranges
         tree
         (ts-parser-parse-string parser tree new-source)))))
  (let* ((source "[11]")
         (parser (make <ts-parser> #:language json-language))
         (_ (test-equal "ts-parser-logger: unset"
              #f (ts-parser-logger parser)))
         (logs '())
         (log-proc (lambda (lv str)
                     (set! logs (cons (cons lv str) logs))))
         (_ (set! (ts-parser-logger parser) log-proc))
         (tree (ts-parser-parse-string parser #f source)))

    (test-equal "ts-parser-logger: get"
      log-proc (ts-parser-logger parser))

    (test-assert "ts-parser-logger: success"
      (not (null? logs)))

    (test-equal "ts-parser-logger: set to #f"
      #f
      (begin (set! (ts-parser-logger parser) #f)
             (ts-parser-logger parser))))
  (let* ((source "[1,null]")
         (parser (make <ts-parser> #:language json-language))
         (tree (ts-parser-parse-string parser #f source))
         (root (ts-tree-root-node tree)))
    (test-assert "write <ts-node>"
      (string-contains
       (call-with-output-string
         (lambda (o)
           (write root o)))
       "\"document\" in"))
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

    (test-equal "ts-node-child-count: 1"
      1 (ts-node-child-count root))
    (test-equal "ts-node-child-count: 2"
      5 (ts-node-child-count (ts-node-child root 0)))
    (test-equal "ts-node-child-count: named 1"
      1 (ts-node-child-count root #t))
    (test-equal "ts-node-child-count: named 2"
      2 (ts-node-child-count (ts-node-child root 0) #t))

    (test-equal "ts-node-childs: 1"
      1 (length (ts-node-childs root #f)))
    (test-equal "ts-node-childs: 2"
      5 (length (ts-node-childs
                 (ts-node-child root 0) #f)))
    (test-equal "ts-node-childs: no provide NAMED? 1"
      1 (length (ts-node-childs root)))
    (test-equal "ts-node-childs: no provide NAMED? 2"
      5 (length (ts-node-childs
                 (ts-node-child root 0))))
    (test-equal "ts-node-childs: named 1"
      1 (length (ts-node-childs root #t)))
    (test-equal "ts-node-childs: named 2"
      2 (length (ts-node-childs
                 (ts-node-child root 0)
                 #t)))

    (let ((node (ts-node-child
                 (ts-node-child root 0) 0 #t) ))
      (test-equal "substring/read-only"
        "1"
        (substring/read-only
         source
         (ts-node-start-byte node)
         (ts-node-end-byte node))))
    (test-equal "ts-node-start-byte"
      0 (ts-node-start-byte root))
    (test-equal "ts-node-end-byte"
      (string-length source) (ts-node-end-byte root))
    (test-equal "ts-node-start-point"
      (cons 0 0) (ts-node-start-point root))
    (test-equal "ts-node-end-point"
      (cons 0 (string-length source)) (ts-node-end-point root))

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
      (ts-node-descendant-for-byte-range (ts-node-child root 0) 0 1 #t)))
  (let* ((source "[1")
         (parser (make <ts-parser>
                   #:language json-language))
         (tree (ts-parser-parse-string parser #f source))
         (root (ts-tree-root-node tree)))
    (test-assert "ts-node-error"
      (ts-node-has-error? root))
    (test-assert "ts-node-sexp: error string"
      (ts-node-sexp root)))

  (let* ((source "[11]")
         (parser (make <ts-parser> #:language json-language)))
    (test-assert "ts-parser-print-dot-graphs"
      (let ((port #f))
        (dynamic-wind
          (lambda () (set! port (mkstemp "test-ts-parser-print-dot-graphs-XXXXXX")))
          (lambda ()
            (ts-parser-print-dot-graphs parser port)
            (ts-parser-parse-string parser #f source)
            (call-with-input-file (port-filename port) get-string-all))
          (lambda () (delete-file (port-filename port))))))))

(test-group "util"
  (let ((str "conf='…'\n"))
    (test-equal "substring-utf8"
      str
      (substring-utf8 str 0 (string-utf8-length str)))))
