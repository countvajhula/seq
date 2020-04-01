#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         racket/generic
         racket/undefined
         racket/set
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
         (contract-out
          [take-while (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [take-until (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [drop-while (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [drop-until (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [split-when (->* ((-> any/c boolean?) sequence?)
                           (#:trim? boolean?)
                           (sequenceof sequence?))]
          [split (->* (any/c
                       sequence?)
                      (#:key (or/c (-> comparable? comparable?)
                                   #f)
                       #:trim? boolean?)
                      (sequenceof sequence?))]
          [split-at (-> exact-positive-integer?
                        sequence?
                        (values sequence? sequence?))]
          [split-where (-> (-> any/c boolean?)
                           sequence?
                           (values sequence? sequence?))]
          [deduplicate (->* (sequence?)
                            (#:key (or/c (-> comparable? comparable?)
                                         #f))
                            list?)]
          [slide (-> exact-positive-integer?
                     sequence?
                     (sequenceof list?))]
          [starts-with? (->* (sequence? sequence?)
                             (#:key (or/c (-> comparable? comparable?)
                                          #f))
                             boolean?)]
          [ends-with? (->* (sequence? sequence?)
                           (#:key (or/c (-> comparable? comparable?)
                                        #f))
                           boolean?)]
          [find (->* (sequence? sequence?)
                     (exact-nonnegative-integer?
                      #:key (or/c (-> comparable? comparable?)
                                  #f))
                     (or/c exact-nonnegative-integer?
                           #f))]
          [replace (->* (sequence? sequence? sequence?)
                        (#:key (or/c (-> comparable? comparable?)
                                     #f)
                         #:how-many (and/c integer?
                                           (>=/c 0)))
                        sequence?)]
          [contains? (->* (sequence? sequence?)
                          (#:key (or/c (-> comparable? comparable?)
                                       #f))
                          boolean?)]
          [trim-if (->* ((-> any/c boolean?)
                         sequence?)
                        (#:side (one-of/c 'left
                                          'right
                                          'both)
                         #:how-many (or/c (and/c integer?
                                                 (>=/c 0))
                                          #f))
                        sequence?)]
          [trim (->* (any/c
                      sequence?)
                     (#:key (or/c (-> comparable? comparable?)
                                  #f)
                      #:side (one-of/c 'left
                                       'right
                                       'both)
                      #:how-many (or/c (and/c integer?
                                              (>=/c 0))
                                       #f))
                     sequence?)]
          [index-of (->* (any/c sequence?)
                         (#:key (or/c (-> comparable? comparable?)
                                      #f))
                         (or/c (and/c integer?
                                      (>=/c 0))
                               #f))]
          [remove (->* (any/c sequence?)
                       (#:key (or/c (-> comparable? comparable?)
                                    #f)
                        #:how-many (or/c (and/c integer?
                                                (>=/c 0))
                                         #f))
                       sequence?)]
          [remove-when (->* ((-> any/c boolean?)
                             sequence?)
                            (#:how-many (or/c (and/c integer?
                                                     (>=/c 0))
                                              #f))
                            sequence?)]
          [add-between (-> any/c
                           sequence?
                           sequence?)]
          [weave (->* (sequence?)
                      (any/c)
                      (or/c sequence?
                            procedure?))] ; procedure doesn't implement sequence
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

(define (take-while pred seq)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (take-while pred vs))
            null))))

(define (drop-while pred seq)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (drop-while pred vs)
            seq))))

(define (take-until pred seq)
  (take-while (!! pred) seq))

(define (drop-until pred seq)
  (drop-while (!! pred) seq))

(define (~split-when pred seq)
  (if (empty? seq)
      (stream (list))
      (let-values ([(chunk remaining) (split-where pred seq)])
        (if (empty? remaining)
            (stream-cons chunk empty-stream)
            (stream-cons chunk
                         (~split-when pred
                                      (rest remaining)))))))

(define (split-when #:trim? [trim? #t]
                    pred
                    seq)
  (let ([result (~split-when pred
                             (if trim?
                                 (trim-if pred seq)
                                 seq))])
    (if (string? seq)
        (map ->string result)
        result)))

(define (split #:key [key #f]
               #:trim? [trim? #t]
               elem
               seq)
  (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
    (split-when #:trim? trim?
                (curry = #:key key elem)
                seq)))

(define (split-at pos seq)
  (let ([left (take pos seq)]
        [right (drop pos seq)])
    (if (string? seq)
        (values (->string left) (->string right))
        (values left right))))

(define (split-where pred seq)
  (let ([left (take-until pred seq)]
        [right (drop-until pred seq)])
    (if (string? seq)
        (values (->string left) (->string right))
        (values left right))))

(define (deduplicate seq #:key [key #f])
  (->list
   (apply generic-set
          #:key key
          seq)))

(define (slide window-size seq)
  ;; TODO: improve; support move-by
  (let ([seqs (for/list ([i (in-range window-size)])
                (drop i seq))])
    (apply map list seqs)))

(define (starts-with? #:key [key #f] prefix seq)
  (if (empty? prefix)
      #t
      (if (empty? seq)
          #f
          (and (= #:key key
                  (first seq)
                  (first prefix))
               (starts-with? #:key key
                             (rest prefix)
                             (rest seq))))))

(define (ends-with? #:key [key #f] suffix seq)
  (starts-with? #:key key
                (reverse suffix)
                (reverse seq)))

(define (find #:key [key #f] subseq seq [idx 0])
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
                                    remaining-subseq
                                    remaining-seq)
                      idx
                      (find #:key key
                            subseq
                            (rest seq)
                            (add1 idx))))
                (find #:key key
                      subseq
                      (rest seq)
                      (add1 idx)))))))

(define (~replace #:key [key #f]
                  #:how-many [how-many #f]
                  orig-subseq
                  new-subseq
                  seq)
  (if (or (not how-many)
          (> how-many 0))
      (let ([idx (find #:key key
                       orig-subseq
                       seq)])
        (if idx
            (.. (take idx seq)
                new-subseq
                (~replace #:key key
                          orig-subseq
                          new-subseq
                          (drop (+ idx
                                   (length orig-subseq))
                                seq)
                          #:how-many (and how-many (sub1 how-many))))
            seq))
      seq))

(define (replace #:key [key #f]
                 #:how-many [how-many #f]
                 orig-subseq
                 new-subseq
                 seq)
  (let ([result (~replace #:key key
                          #:how-many how-many
                          orig-subseq
                          new-subseq
                          seq)])
    (if (string? seq)
        (->string result)
        result)))

(define (contains? #:key [key #f] subseq seq)
  (->boolean (find #:key key subseq seq)))

(define (trim-left-if pred
                      seq
                      #:how-many [how-many #f])
  (if (empty? seq)
      seq
      (let ([v (first seq)])
        (if (or (not how-many)
                (> how-many 0))
            (if (pred v)
                (trim-left-if pred
                              (rest seq)
                              #:how-many (and how-many (sub1 how-many)))
                seq)
            seq))))

(define (trim-right-if pred
                       seq
                       #:how-many [how-many #f])
  (reverse (trim-left-if pred
                         (reverse seq)
                         #:how-many how-many)))

(define (~trim-if pred
                  seq
                  #:side [side 'both]
                  #:how-many [how-many #f])
  (let ([seq (if (member? side '(left both))
                 (trim-left-if pred
                               seq
                               #:how-many how-many)
                 seq)])
    (if (member side '(right both))
        (trim-right-if pred
                       seq
                       #:how-many how-many)
        seq)))

(define (trim-if pred
                 seq
                 #:side [side 'both]
                 #:how-many [how-many #f])
  (let ([result (~trim-if pred
                          seq
                          #:side side
                          #:how-many how-many)])
    (if (string? seq)
        (->string result)
        result)))

(define (trim elem
              seq
              #:key [key #f]
              #:side [side 'both]
              #:how-many [how-many #f])
  (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
    (trim-if (curry = #:key key elem)
             seq
             #:side side
             #:how-many how-many)))

(define (index-of #:key [key #f]
                  elem
                  seq)
  (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
    (d:index-of seq
                elem
                (curry = #:key key))))

(define (~remove-when #:how-many [how-many #f]
                      pred
                      seq)
  (if ((|| set? gset?) seq)
      (raise-argument-error 'remove-when
                            "sequence? that is not a pure set"
                            seq)
      (if (empty? seq)
          seq
          (if how-many
              (if (> how-many 0)
                  (let ([v (first seq)]
                        [vs (rest seq)])
                    (if (pred v)
                        (~remove-when #:how-many (sub1 how-many)
                                      pred
                                      (rest seq))
                        (stream-cons v
                                     (~remove-when #:how-many how-many
                                                   pred
                                                   (rest seq)))))
                  seq)
              (filter (!! pred) seq)))))

(define (remove-when #:how-many [how-many #f]
                      pred
                      seq)
  (let ([result (~remove-when #:how-many how-many
                              pred
                              seq)])
    (if (string? seq)
        (->string result)
        result)))

(define (remove #:key [key #f]
                #:how-many [how-many #f]
                elem
                seq)
  (if ((|| set? gset?) seq)
      (set-remove seq elem)
      (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
        (remove-when #:how-many how-many
                     (curry = #:key key elem)
                     seq))))

(define (add-between sep seq)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (empty? vs)
            (stream v)
            (stream-cons v
                         (stream-cons sep
                                      (add-between sep vs)))))))

(define (weave seq [sep undefined])
  (fold .. (if (undefined? sep)
               seq
               (add-between sep seq))))
