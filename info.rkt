#lang info
(define collection "seq")
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
(define scribblings '(("scribblings/seq.scrbl" ())))
(define compile-omit-paths '("dev" "tests" "coverage"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc" "doc/seq" "tests/compiled" "tests/private/compiled"))
(define pkg-desc "A generic sequence library exhibiting functional and lazy semantics.")
(define version "0.0")
(define pkg-authors '(countvajhula))
