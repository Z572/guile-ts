(define-module (guile-ts-package)
  #:use-module (gnu packages tree-sitter)
  #:use-module (rnrs io ports)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gettext)
  #:use-module (guix gexp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages freedesktop))

(define %srcdir (string-append
                 (current-source-directory)
                 "/../.."))
(define %version (string-append
                  (call-with-input-file (string-append %srcdir "/" "version")
                    get-string-all)
                  "-git"))

(define source-checkout
  (local-file
   "../.." (git-file-name "guile-ts" %version)
   #:recursive? #t
   #:select? (or (git-predicate %srcdir)
                 (const #t))))
(define-public guile-ts
  (package
    (name "guile-ts")
    (version %version)
    (source source-checkout)
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-extension-path
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute*
                       (find-files "." ".*\\.scm")
                     (("\\(load-extension \"libguile_ts\" *\"(.*)\"\\)" _ o)
                      (string-append
                       (object->string
                        `(or (false-if-exception
                              (load-extension "libguile_ts" ,o))
                             (load-extension
                              ,(string-append
                                #$output
                                "/lib/libguile_ts.so")
                              ,o)))))))))))
    (native-inputs
     (list autoconf automake libtool texinfo pkg-config guile-3.0))
    (inputs
     (list guile-3.0 tree-sitter))
    (native-search-paths
     (list (search-path-specification
            (variable "TREE_SITTER_GRAMMAR_PATH")
            (files '("lib/tree-sitter")))))
    (synopsis "Guile bindings to the Tree-sitter parsing library")
    (description "This package provides Guile bindings to the Tree-sitter
parsing library.")
    (home-page "https://github.com/Z572/guile-ts")
    (license license:gpl3+)))

(define source-tarball
  ;; Tarball make from the Git checkout.
  (dist-package guile-ts source-checkout
                #:phases #~(modify-phases %dist-phases
                             (replace 'build-dist
                               (lambda args
                                 ;; Run "make" before "make distcheck".
                                 (apply (assoc-ref %dist-phases 'build-dist)
                                        #:build-before-dist? #t
                                        #:dist-target "dist"
                                        #:tests? #f
                                        args))))))

(define-public guile-ts-from-tarball
  (let ((base guile-ts))
    (package
      (inherit guile-ts)
      (version (string-append %version "-tarball"))
      (source source-tarball)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (replace 'unpack
                (lambda _
                  (define source
                    #+(package-source this-package))

                  ;; Locate a tarball within SOURCE and unpack it.
                  (invoke "tar" "xvf"
                          (car (find-files source "\\.tar.gz$")))
                  (let ((directory
                         (car (find-files "."
                                          (lambda (file stat)
                                            (and (string-prefix?
                                                  "guile-ts" (basename file))
                                                 (eq? 'directory
                                                      (stat:type stat))))
                                          #:directories? #t))))
                    (format #t "changing directory to '~a'~%" directory)
                    (chdir directory))))))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (delete "autoconf" "automake"))))))

(define tree-sitter-0.24
  (let ((base tree-sitter))
    (package
      (inherit base)
      (name (package-name base))
      (version "0.24.6")
      (source (origin
                (inherit (package-source base))
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tree-sitter/tree-sitter")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hp0cmqvrh8f2mfs0hhn9n1dr53vigxcb88xfj27m4mn5pypdc9g")))))))

(define-public guile-ts/ts-0.24
  (let ((base guile-ts))
    (package
      (inherit base)
      (name "guile-ts-with-tree-sitter-0.24")
      (inputs
       (modify-inputs (package-inputs base)
         (replace "tree-sitter" tree-sitter-0.24))))))

guile-ts
