(define-module (ts query)
  #:use-module (ts api)
  #:use-module (oop goops)
  #:export (ts-query-new
            ts-query-pattern-rooted?
            ts-query-cursor-new
            ts-query-cursor-exec
            ts-query-pattern-count
            ts-query-capture-count
            ts-query-string-count
            ts-query-capture-name-for-id
            ts-query-string-value-for-id
            ts-query-capture-quantifier-for-id
            ts-query-start-byte-for-pattern
            ts-query-cursor-next-match
            ts-query-match-id
            ts-query-match-pattern-index
            ts-query-match-captures))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_query"))

(define-class <ts-query-match> ()
  (id #:getter ts-query-match-id)
  (pattern-index #:getter ts-query-match-pattern-index)
  (captures #:getter ts-query-match-captures))
