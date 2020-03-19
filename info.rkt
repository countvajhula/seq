#lang info
(define collection "collection-utils")
(define deps '("base"
               "collections-lib"
               "functional-utils"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"))
(define scribblings '(("scribblings/collection-utils.scrbl" (multi-page))))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for collections")
(define version "0.0")
(define pkg-authors '(countvajhula))
