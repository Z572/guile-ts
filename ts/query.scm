(define-module (ts query)
  #:use-module (ts api)
  #:export (ts-query-new
            ts-query-delete
            ts-query-pattern-rooted?
            ts-query-cursor-new
            ts-query-cursor-delete
            ts-query-cursor-exec))

(eval-when (expand load eval)
  (load-extension "libguile_ts" "init_ts_query"))
