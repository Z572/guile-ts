(define-module (ts))

(eval-when (eval load compile)
  (begin
    (define %public-modules
      '((ts init)
        (ts api)
        (ts parser)
        (ts tree)
        (ts language)
        (ts query)))

    (let* ((current-module (current-module))
           (current-module-interface (resolve-interface (module-name current-module))))
      (for-each
       (lambda (submodule)
         (let ((submodule-interface (resolve-interface submodule)))
           (module-use! current-module submodule-interface)
           (module-use! current-module-interface submodule-interface)))
       %public-modules)
      (set-module-duplicates-handlers!
       current-module-interface
       (append (lookup-duplicates-handlers 'merge-accessors)
               (lookup-duplicates-handlers 'merge-generics)
               (module-duplicates-handlers current-module-interface))))))
