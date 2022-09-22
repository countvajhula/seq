#lang info

(define collection "seq")
(define deps '("base"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-doc"
                     "functional-doc"
                     "relation"
                     "sandbox-lib"))
(define scribblings '(("scribblings/seq.scrbl" ())))
(define clean '("compiled" "doc" "doc/seq"))
(define pkg-desc "A generic sequence library exhibiting functional and lazy semantics.")
(define version "0.0")
(define pkg-authors '(countvajhula))
