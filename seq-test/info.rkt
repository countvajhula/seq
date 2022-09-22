#lang info

(define collection "seq")
(define deps '("base"))
(define build-deps '("rackunit-lib"
                     "cover"
                     "cover-coveralls"
                     "relation-lib"))
(define clean '("compiled" "tests/compiled" "tests/private/compiled"))
(define pkg-desc "A generic sequence library exhibiting functional and lazy semantics.")
(define version "0.0")
(define pkg-authors '(countvajhula))
