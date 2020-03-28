#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         (only-in data/collection
                  (index-of d:index-of))
         (only-in algebraic/prelude
                  &&
                  ||)
         functional-utils
         core-utils
         relation)

(provide every
         takef
         dropf
         split-at
         splitf-at
         deduplicate
         slide
         starts-with?
         ends-with?
         find
         replace
         contains?
         trim
         index-of
         remove
         add-between
         weave
         (contract-out
          [: (collection? any/c . -> . collection?)]))

(define : conj)

(define (every cnt seq)
  (if (empty? seq)
      empty-stream
      (let ([head (first seq)]
            [tail (with-handlers
                    ([exn:fail?
                      (λ (exn)
                        empty-stream)])
                    (drop cnt seq))])
        (stream-cons head (every cnt tail)))))

(define (takef seq pred)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (takef vs pred))
            null))))

(define (dropf seq pred)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (dropf vs pred)
            seq))))

(define (split-at seq pos)
  ;; TODO: make this more efficient
  (values (take pos seq) (drop pos seq)))

(define (splitf-at seq pred)
  ;; TODO: make this more efficient
  (values (takef seq pred) (dropf seq pred)))

(define (deduplicate seq #:key [key #f])
  (apply generic-set
         #:key key
         seq))

(define (slide seq
               [window-size 1])
  ;; TODO: improve; support move-by
  (let ([seqs (for/list ([i (in-range window-size)])
                (drop i seq))])
    (apply map list seqs)))

(define (starts-with? #:key [key #f] seq prefix)
  (if (empty? prefix)
      #t
      (if (empty? seq)
          #f
          (and (= #:key key
                  (first seq)
                  (first prefix))
               (starts-with? #:key key
                             (rest seq)
                             (rest prefix))))))

(define (ends-with? #:key [key #f] str suffix)
  (starts-with? #:key key
                (reverse str)
                (reverse suffix)))

(define (find #:key [key #f] seq subseq [idx 0])
  (if (empty? subseq)
      0
      (if (empty? seq)
          #f
          (let ([v (first seq)]
                [w (first subseq)])
            (if (= #:key key v w)
                (let ([remaining-seq (rest seq)]
                      [remaining-subseq (rest subseq)])
                  (if (starts-with? #:key key
                                    remaining-seq
                                    remaining-subseq)
                      idx
                      (find #:key key
                            (rest seq)
                            subseq
                            (add1 idx))))
                (find #:key key
                      (rest seq)
                      subseq
                      (add1 idx)))))))

(define (replace #:key [key #f]
                 #:how-many [how-many #f]
                 seq
                 orig-subseq
                 new-subseq)
  (if (or (not how-many)
          (> how-many 0))
      (let ([idx (find #:key key
                       seq
                       orig-subseq)])
        (if idx
            (.. (take idx seq)
                new-subseq
                (replace #:key key
                         (drop (+ idx
                                  (length orig-subseq))
                               seq)
                         orig-subseq
                         new-subseq
                         #:how-many (and how-many (sub1 how-many))))
            seq))
      seq))

(define (contains? #:key [key #f] seq subseq)
  (->boolean (find #:key key seq subseq)))

(define (trim-left seq
                   pred
                   #:how-many [how-many #f])
  (let ([v (first seq)])
    (if (or (not how-many)
            (> how-many 0))
        (if (pred v)
            (trim-left (rest seq)
                       pred
                       #:how-many (and how-many (sub1 how-many)))
            seq)
        seq)))

(define (trim-right seq
                    pred
                    #:how-many [how-many #f])
  (reverse (trim-left (reverse seq)
                      pred
                      #:how-many how-many)))

(define (trim seq
              pred
              #:left? [left? #t]
              #:right? [right? #t]
              #:how-many [how-many #f])
  (let ([seq (if left?
                 (trim-left seq
                            pred
                            #:how-many how-many)
                 seq)])
    (if right?
        (trim-right seq
                    pred
                    #:how-many how-many)
        seq)))

(define (index-of #:key [key #f]
                  seq
                  elem)
  (d:index-of seq
              elem
              (curry = #:key key)))

(define (remove #:key [key #f]
                #:how-many [how-many #f]
                seq
                elem)
  (if how-many
      (if (> how-many 0)
          (let ([result (remove-first seq
                                      elem
                                      (curry = #:key key))])
            (remove #:key key
                    #:how-many (sub1 how-many)
                    result elem))
          seq)
      (remove-all seq
                  elem
                  (curry = #:key key))))

(define (add-between seq sep)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (empty? vs)
            (stream v)
            (stream-cons v
                         (stream-cons sep
                                      (add-between vs sep)))))))

(define (weave seq [sep undefined])
  (fold .. (if (undefined? sep)
               seq
               (add-between seq sep))))
