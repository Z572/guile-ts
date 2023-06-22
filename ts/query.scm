(define-module (ts query)
  #:use-module (ts api)
  #:export (ts-query-new
            ts-query-pattern-rooted?
            ts-query-cursor-new
            ts-query-cursor-exec
            ts-query-pattern-count
            ts-query-capture-count
            ts-query-string-count
            ts-query-capture-name-for-id
            ts-query-string-value-for-id))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_query"))
