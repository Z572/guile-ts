(use-modules
 (gnu packages tree-sitter)
 (guix utils) (guix packages)
 ((guix licenses) #:prefix license:)
 (gnu packages xorg)
 (guix download)
 (guix git-download)
 (gnu packages gettext)
 (guix gexp)
 (gnu packages gl)
 (gnu packages xdisorg)
 (guix build-system gnu)
 (gnu packages bash)
 (gnu packages)
 (gnu packages autotools)
 (gnu packages guile)
 (gnu packages gtk)
 (gnu packages guile-xyz)
 (gnu packages ibus)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (gnu packages wm)
 (gnu packages freedesktop))

(define %srcdir
  (dirname (current-filename)))

(package
  (inherit guile-ts)
  (name "guile-ts")
  (version "0.2.0")
  (source
   (local-file
    "." "guile-ts"
    #:recursive? #t
    #:select? (git-predicate %srcdir))))
