#lang racket/base

(require racket/generic
         racket/contract
         data/collection
         (only-in relation true.))

(provide
 (contract-out
  [struct finite-sequence ((seq sequence?))]))

(struct finite-sequence (seq)
  #:transparent
  #:methods gen:countable
  [(define/generic -length length)
   (define (length this)
     (-length (finite-sequence-seq this)))
   (define known-finite? true.)]
  #:methods gen:sequence
  [(define/generic -empty? empty?)
   (define/generic -first first)
   (define/generic -rest rest)
   (define/generic -nth nth)
   (define/generic -reverse reverse)
   (define/generic -random-access? random-access?)
   (define (empty? this)
     (-empty? (finite-sequence-seq this)))
   (define (first this)
     (-first (finite-sequence-seq this)))
   (define (rest this)
     (-rest (finite-sequence-seq this)))
   (define (nth this index)
     (-nth (finite-sequence-seq this)
           index))
   (define (reverse this)
     (-reverse (finite-sequence-seq this)))
   (define (random-access? this)
     (-random-access? (finite-sequence-seq this)))])
