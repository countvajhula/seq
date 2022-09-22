#lang info

(define collection "seq")
(define deps '("base"))
(define build-deps '("seq-lib"
                     "scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "collections-lib"
                     "collections-doc"
                     "functional-doc"
                     "relation-lib"
                     "relation-doc"
                     "sandbox-lib"))
(define scribblings '(("scribblings/seq.scrbl" ())))
(define clean '("compiled" "doc" "doc/seq"))
(define pkg-desc "A generic sequence library exhibiting functional and lazy semantics.")
(define version "0.0")
(define pkg-authors '(countvajhula))
