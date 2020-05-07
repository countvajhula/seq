#lang info
(define collection "collection-util")
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"))
(define scribblings '(("scribblings/collection-util.scrbl" (multi-page))))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for collections")
(define version "0.0")
(define pkg-authors '(countvajhula))
