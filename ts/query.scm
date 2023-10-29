(define-module (ts query)
  #:use-module (ts init)
  #:use-module (ts util)
  #:use-module (ts language)
  #:use-module (system foreign-object)
  #:use-module ((system foreign) #:select (make-pointer))
  #:use-module (oop goops)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-71)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:export (&ts-query-syntax-error
            ts-query-syntax-error?
            ts-query-syntax-error-source
            ts-query-syntax-error-offset
            ts-query-syntax-error-type

            ts-query-new
            ts-query-pattern-rooted?
            ts-query-predicates-for-pattern
            ts-query-cursor-new
            ts-query-cursor-exec
            ts-query-pattern-count
            ts-query-capture-count
            ts-query-string-count
            ts-query-capture-name-for-id
            ts-query-string-value-for-id
            ts-query-capture-quantifier-for-id
            ts-query-start-byte-for-pattern
            ts-query-cursor-set-byte-range!
            ts-query-cursor-next-match
            ts-query-cursor-remove-match
            ts-query-cursor-next-capture
            ts-query-match-id
            ts-query-match-pattern-index
            ts-query-match-captures))

(define-condition-type &ts-query-syntax-error &error
  ts-query-syntax-error?
  (source ts-query-syntax-error-source)
  (offset ts-query-syntax-error-offset)
  (type ts-query-syntax-error-type))

(define (%ts-query-delete obj)
  (let ((%data (slot-ref obj '%data)))
    (%ts_query_delete (make-pointer %data))))

(define (%ts-query-cursor-delete obj)
  (let ((%data (slot-ref obj '%data)))
    (%ts_query_cursor_delete (make-pointer %data))))

(define <ts-query>
  (make-foreign-object-type
   '<ts-query> '(%data)
   #:finalizer %ts-query-delete))

(define <ts-query-cursor>
  (make-foreign-object-type
   '<ts-query-cursor> '(%data)
   #:finalizer %ts-query-cursor-delete))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_query"))

(define (ts-query-new language . sources)
  (let* ((str
          (string-concatenate
           (map
            (lambda (source)
              (cond ((string? source) source)
                    ((list? source) (ts-pattern-expand source))
                    (else (raise
                           (condition
                            (&message (message "SOURCE is not string or list!"))
                            (&error))))))
            sources)))
         (query offset type (%ts-query-new
                             language
                             str)))
    (or query
        (raise (condition
                (&ts-query-syntax-error (source str)
                                        (offset offset)
                                        (type type)))))))
(define (ts-pattern-expand o)
  (match o
    (#:anchor ".")
    (#:? "?")
    (#:* "*")
    (#:+ "+")
    ((? symbol? rest)
     (symbol->string rest))
    ((? keyword? rest)
     (string-append "#" (symbol->string (keyword->symbol rest))))
    ((? string? rest)
     (string-append
      "\""
      (reverse-list->string
       (string-fold
        (lambda (a b)
          (append
           (case a
             ((#\nul) (list #\0 #\\))
             ((#\tab) (list #\t #\\))
             ((#\return) (list #\r #\\))
             ((#\nl) (list #\n #\\))
             ((#\") (list #\" #\\))
             ((#\\) (list #\\ #\\))
             (else => list))
           b)) '() rest))
      "\""))
    ((rest ...)
     (string-append "(" (string-join (map ts-pattern-expand rest)) ")"))
    (#(rest ...)
     (string-append "[" (string-join (map ts-pattern-expand rest)) "]"))))

(define-class <ts-query-match> ()
  (id #:getter ts-query-match-id)
  (pattern-index #:getter ts-query-match-pattern-index)
  (captures #:getter ts-query-match-captures))

(define (ts-query-predicates-for-pattern self index)
  (let ((steps (%ts-query-predicates-for-pattern self index)))
    (list-transduce (compose
                     (tpartition (lambda (o) (eq? (car o) 'done)))
                     (tremove (lambda (o) (equal? o '((done . #f))))))
                    rcons steps)))
