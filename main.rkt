#lang racket/base

(require (prefix-in b: racket/base)
         racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/generic
         racket/undefined
         racket/set
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         (only-in data/collection
                  (index-of d:index-of)
                  (append d:append))
         relation)

(provide (contract-out
          [every (-> exact-positive-integer? sequence? sequence?)]
          [exists (->i ([pred (seqs)
                              (and/c (procedure-arity-includes/c (b:length seqs))
                                     (unconstrained-domain-> boolean?))])
                       #:rest [seqs (listof (sequenceof any/c))]
                       [result any/c])]
          [for-all (->i ([pred (seqs)
                               (and/c (procedure-arity-includes/c (b:length seqs))
                                      (unconstrained-domain-> boolean?))])
                        #:rest [seqs (listof (sequenceof any/c))]
                        [result any/c])]
          [zip-with (->* (procedure? sequence?)
                         #:rest (listof sequence?)
                         sequence?)]
          [zip (-> sequence? sequence? ... sequence?)]
          [unzip-with (->* (procedure? sequence?)
                           sequence?)]
          [unzip (-> sequence? sequence?)]
          [find (->i ([pred (seqs)
                            (and/c (procedure-arity-includes/c (b:length seqs))
                                   (unconstrained-domain-> boolean?))])
                     #:rest [seqs (listof (sequenceof any/c))]
                     [result any/c])]
          [choose (-> (-> any/c boolean?)
                      sequence?
                      ...
                      sequence?)]
          [take-when ((any/c . -> . any/c)
                      sequence?
                      . -> .
                      sequence?)]
          [take-while (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [drop-while (-> (-> any/c boolean?)
                          sequence?
                          sequence?)]
          [take-until (-> (-> any/c boolean?)
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
          [split-by (-> exact-positive-integer?
                        sequence?
                        (sequenceof sequence?))]
          [deduplicate (->* (sequence?)
                            (#:key (or/c (-> comparable? comparable?)
                                         #f))
                            list?)]
          [cascade (-> exact-positive-integer?
                       sequence?
                       (sequenceof sequence?))]
          [slide (-> exact-positive-integer?
                     sequence?
                     (sequenceof sequence?))]
          [prefix-of? (->* (sequence? sequence?)
                             (#:key (or/c (-> comparable? comparable?)
                                          #f))
                             boolean?)]
          [starts-with? (->* (sequence? sequence?)
                             (#:key (or/c (-> comparable? comparable?)
                                          #f))
                             boolean?)]
          [suffix-of? (->* (sequence? sequence?)
                           (#:key (or/c (-> comparable? comparable?)
                                        #f))
                           boolean?)]
          [ends-with? (->* (sequence? sequence?)
                           (#:key (or/c (-> comparable? comparable?)
                                        #f))
                           boolean?)]
          [find-infix (->* (sequence? sequence?)
                           (exact-nonnegative-integer?
                            #:key (or/c (-> comparable? comparable?)
                                        #f))
                           (or/c exact-nonnegative-integer?
                                 #f))]
          [replace-infix (->* (sequence? sequence? sequence?)
                              (#:key (or/c (-> comparable? comparable?)
                                           #f)
                               #:how-many (and/c integer?
                                                 (>=/c 0)))
                              sequence?)]
          [infix-of? (->* (sequence? sequence?)
                          (#:key (or/c (-> comparable? comparable?)
                                       #f))
                          boolean?)]
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
          [trim-by (-> exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       sequence?
                       sequence?)]
          [index-of (->* (any/c sequence?)
                         (#:key (or/c (-> comparable? comparable?)
                                      #f))
                         (or/c (and/c integer?
                                      (>=/c 0))
                               #f))]
          [index (->* (any/c sequence?)
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
          [drop-when (->* ((-> any/c boolean?)
                           sequence?)
                          (#:how-many (or/c (and/c integer?
                                                   (>=/c 0))
                                            #f))
                          sequence?)]
          [add-between (-> any/c
                           sequence?
                           sequence?)]
          [join (->* (sequence?)
                     (any/c)
                     (or/c sequence?
                           procedure?))] ; procedure doesn't implement sequence
          [wrap-each (-> any/c
                         any/c
                         sequence?
                         sequence?)]
          [weave (-> any/c any/c sequence?
                     (or/c sequence?
                           procedure?))] ; procedure doesn't implement sequence
          [interleave (-> sequence? sequence? ... sequence?)]
          [: (collection? any/c . -> . collection?)]))

(define : conj)

(define take-when filter)

(define (every cnt seq)
  (if (empty? seq)
      empty-stream
      (let ([head (first seq)]
            [tail (with-handlers
                    ([exn:fail:contract?
                      (λ (exn)
                        empty-stream)])
                    (drop cnt seq))])
        (stream-cons head (every cnt tail)))))

(define (zip-with op . seqs)
  (if (exists empty? seqs)
      empty-stream
      (let ([vs (map first seqs)])
        (stream-cons (apply op vs)
                     (apply zip-with op (map rest seqs))))))

(define (zip . seqs)
  (apply zip-with list seqs))

;; zip is its own inverse
(define unzip-with (curry apply zip-with))

(define unzip (curry apply zip))

(define exists ormap)

(define for-all andmap)

(define (singleton? seq)
  ;; cheap check to see if a list is of length 1,
  ;; instead of traversing to compute the length
  (if (empty? seq)
      #f
      (with-handlers ([exn:fail? (λ (exn) #t)])
        (and (second seq) #f))))

(define (find pred . seqs)
  (let ([vs (take-when (curry apply pred)
                       (apply zip seqs))])
    (if (empty? vs)
        #f
        (let ([result (first vs)])
          (if (singleton? result)
              (join result)
              result)))))

(define (choose pred . seqs)
  (map (curry find pred) seqs))

(define (take-while pred seq)
  (if (empty? seq)
      empty-stream
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (take-while pred vs))
            null))))

(define (drop-while pred seq)
  (if (empty? seq)
      empty-stream
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
      (stream ID)
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

(define (split-by n seq)
  (every n (slide n seq)))

(define (deduplicate seq #:key [key #f])
  (->list
   (apply generic-set
          #:key key
          seq)))

(define (interleave . seqs)
  (if (empty? seqs)
      empty-stream
      (let loop ([remaining-seqs seqs])
        (if (empty? remaining-seqs)
            (apply interleave (map rest seqs))
            (if (empty? (first remaining-seqs))
                empty-stream
                (stream-cons (first (first remaining-seqs))
                             (loop (rest remaining-seqs))))))))

(define (cascade step-size seq)
  (if (empty? seq)
      empty-stream
      (stream-cons seq
                   (with-handlers ([exn:fail:contract? (λ (exn)
                                                         empty-stream)])
                     (cascade step-size
                              (drop step-size seq))))))

(define (slide window-size seq)
  (let loop ([seq (cascade 1 seq)])
    (if (empty? seq)
        empty-stream
        (let ([window (with-handlers ([exn:fail:contract? (λ (exn)
                                                            empty-stream)])
                        (take window-size (first seq)))])
          (if (empty? window)
              empty-stream
              (stream-cons window (loop (rest seq))))))))

(define (prefix-of? #:key [key #f] prefix seq)
  (if (empty? prefix)
      #t
      (if (empty? seq)
          #f
          (and (= #:key key
                  (first seq)
                  (first prefix))
               (prefix-of? #:key key
                             (rest prefix)
                             (rest seq))))))

(define starts-with? prefix-of?)

(define (suffix-of? #:key [key #f] suffix seq)
  (prefix-of? #:key key
                (reverse suffix)
                (reverse seq)))

(define ends-with? suffix-of?)

(define (find-infix #:key [key #f] subseq seq [idx 0])
  (if (empty? subseq)
      0
      (if (empty? seq)
          #f
          (let ([v (first seq)]
                [w (first subseq)])
            (if (= #:key key v w)
                (let ([remaining-seq (rest seq)]
                      [remaining-subseq (rest subseq)])
                  (if (prefix-of? #:key key
                                  remaining-subseq
                                  remaining-seq)
                      idx
                      (find-infix #:key key
                                  subseq
                                  (rest seq)
                                  (add1 idx))))
                (find-infix #:key key
                            subseq
                            (rest seq)
                            (add1 idx)))))))

(define (~replace-infix #:key [key #f]
                        #:how-many [how-many #f]
                        orig-subseq
                        new-subseq
                        seq)
  (if (or (not how-many)
          (> how-many 0))
      (let ([idx (find-infix #:key key
                             orig-subseq
                             seq)])
        (if idx
            (.. (take idx seq)
                new-subseq
                (~replace-infix #:key key
                                orig-subseq
                                new-subseq
                                (drop (+ idx
                                         (length orig-subseq))
                                      seq)
                                #:how-many (and how-many (sub1 how-many))))
            seq))
      seq))

(define (replace-infix #:key [key #f]
                       #:how-many [how-many #f]
                       orig-subseq
                       new-subseq
                       seq)
  (let ([result (~replace-infix #:key key
                                #:how-many how-many
                                orig-subseq
                                new-subseq
                                seq)])
    (if (string? seq)
        (->string result)
        result)))

(define (infix-of? #:key [key #f] subseq seq)
  (->boolean (find-infix #:key key subseq seq)))

(define contains? infix-of?)

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

(define (trim-by left right seq)
  (let ([len (length seq)])
    (if (> (+ left right)
           len)
        (raise-arguments-error 'trim-by
                               "Trimmed lengths exceed total length!"
                               "left" left
                               "right" right
                               "sequence length" len)
        (take (- len
                 (+ left
                    right))
              (drop left seq)))))

(define (index-of #:key [key #f]
                  elem
                  seq)
  (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
    (d:index-of seq
                elem
                (curry = #:key key))))

(define index index-of)

(define (~drop-when #:how-many [how-many #f]
                    pred
                    seq)
  (if ((|| set? gset?) seq)
      (raise-argument-error 'drop-when
                            "sequence? that is not a pure set"
                            seq)
      (if (empty? seq)
          seq
          (if how-many
              (if (> how-many 0)
                  (let ([v (first seq)]
                        [vs (rest seq)])
                    (if (pred v)
                        (~drop-when #:how-many (sub1 how-many)
                                    pred
                                    (rest seq))
                        (stream-cons v
                                     (~drop-when #:how-many how-many
                                                 pred
                                                 (rest seq)))))
                  seq)
              (take-when (!! pred) seq)))))

(define (drop-when #:how-many [how-many #f]
                   pred
                   seq)
  (let ([result (~drop-when #:how-many how-many
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
        (drop-when #:how-many how-many
                   (curry = #:key key elem)
                   seq))))

(define (add-between sep seq)
  (if (empty? seq)
      empty-stream
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (empty? vs)
            (stream v)
            (stream-cons v
                         (stream-cons sep
                                      (add-between sep vs)))))))

(define (wrap-each before after seq)
  (if (empty? seq)
      empty-stream
      (let ([v (first seq)]
            [vs (rest seq)])
        (let ([wrapped-v (stream before v after)])
          (if (empty? vs)
              wrapped-v
              (stream-append wrapped-v
                             (wrap-each before after vs)))))))

(define (join seq [sep undefined])
  (fold .. (if (undefined? sep)
               seq
               (add-between sep seq))))

(define (weave to from seq)
  (fold .. (wrap-each to from seq)))
