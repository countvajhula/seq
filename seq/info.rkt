#lang info

(define collection 'multi)
(define deps '("base"
               "seq-lib"
               "seq-doc"
               "seq-test"))
(define build-deps '())
(define implies '("seq-lib"
                  "seq-doc"
                  "seq-test"))