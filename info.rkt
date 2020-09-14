#lang info
(define collection "collection-util")
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-doc"
                     "rackunit-lib"
                     "cover"
                     "cover-coveralls"
                     "relation"
                     "sandbox-lib"))
(define scribblings '(("scribblings/collection-util.scrbl" ())))
(define compile-omit-paths '("dev" "test.rkt" "coverage"))
(define test-include-paths '("test.rkt"))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for collections")
(define version "0.0")
(define pkg-authors '(countvajhula))
