#lang racket/base

(require (prefix-in b: racket/base)
         (except-in racket/contract
                    predicate/c)
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
                    index-of
                    index-where)
         (only-in data/collection
                  [index-of d:index-of]
                  [append d:append])
         relation
         contract/social)

(provide take-when
         prefix
         suffix-at
         infix
         infix-at
         exists
         for-all
         (contract-out
          [by (map/c (head exact-positive-integer?))]
          [init (function/c (nonempty/c sequence?) sequence?)]
          [zip-with (->* (procedure? sequence?)
                         #:rest (listof sequence?)
                         sequence?)]
          [zip (variadic-composition/c sequence? (head sequence?))]
          [unzip-with (->* (procedure? sequence?)
                           sequence?)]
          [unzip map/c]
          [find (->i ((pred (seqs)
                            (and/c
                             (procedure-arity-includes/c (b:length seqs))
                             (unconstrained-domain-> boolean?))))
                     #:rest (seqs (listof (sequenceof any/c)))
                     (result any/c))]
          [index-where (->i ((pred (seqs)
                                   (and/c
                                    (procedure-arity-includes/c (b:length seqs))
                                    (unconstrained-domain-> boolean?))))
                            #:rest (seqs (listof (sequenceof any/c)))
                            (result any/c))]
          [choose (variadic-composition/c sequence? (head predicate/c))]
          [suffix (map/c (head exact-nonnegative-integer?))]
          [take-while filter/c]
          [drop-while filter/c]
          [take-until filter/c]
          [drop-until filter/c]
          [cut-when (->* (predicate/c sequence?)
                         (#:trim? boolean?)
                         (sequenceof sequence?))]
          [cut (->* (any/c sequence?)
                    (#:key (maybe/c function/c)
                     #:trim? boolean?)
                    (sequenceof sequence?))]
          [cut-at (binary-function/c exact-positive-integer?
                                     sequence?
                                     (values sequence? sequence?))]
          [cut-where (binary-function/c predicate/c
                                        sequence?
                                        (values sequence? sequence?))]
          [cut-by (lift/c sequence?
                          sequenceof
                          (head exact-positive-integer?))]
          [cut-with (binary-function/c predicate/c
                                       sequence?
                                       (values sequence? sequence?))]
          [truncate (binary-composition/c sequence?)]
          [rotate-left (map/c (head exact-nonnegative-integer?))]
          [rotate-right (map/c (head exact-nonnegative-integer?))]
          [rotate map/c]
          [rotations (lift/c sequence? sequenceof)]
          [deduplicate (->* (sequence?)
                            (#:key (maybe/c function/c))
                            list?)]
          [multiples (->* (number?)
                          (natural-number/c)
                          sequence?)]
          [powers (->* (any/c)
                       (procedure?)
                       sequence?)]
          [iterate (binary-function/c procedure?
                                      any/c
                                      sequence?)]
          [suffixes (lift/c sequence? sequenceof)]
          [prefixes (->* (sequence?)
                         (exact-positive-integer?) ; the optional int is internal
                         (sequenceof sequence?))]
          [infixes (lift/c sequence?
                           sequenceof
                           (head exact-positive-integer?))]
          [prefix? (->* (sequence? sequence?)
                        (#:key (maybe/c function/c))
                        boolean?)]
          [starts-with? (->* (sequence? sequence?)
                             (#:key (maybe/c function/c))
                             boolean?)]
          [suffix? (->* (sequence? sequence?)
                        (#:key (maybe/c function/c))
                        boolean?)]
          [ends-with? (->* (sequence? sequence?)
                           (#:key (maybe/c function/c))
                           boolean?)]
          [find-infix (->* (sequence? sequence?)
                           (exact-nonnegative-integer?
                            #:key (maybe/c function/c))
                           (maybe/c exact-nonnegative-integer?))]
          [replace-infix (->* (sequence? sequence? sequence?)
                              (#:key (maybe/c function/c)
                               #:how-many exact-nonnegative-integer?)
                              sequence?)]
          [infix? (->* (sequence? sequence?)
                       (#:key (maybe/c function/c))
                       boolean?)]
          [contains? (->* (sequence? sequence?)
                          (#:key (maybe/c function/c))
                          boolean?)]
          [trim-if (->* (predicate/c sequence?)
                        (#:side (one-of/c 'left 'right 'both)
                         #:how-many (maybe/c exact-nonnegative-integer?))
                        sequence?)]
          [trim (->* (any/c sequence?)
                     (#:key (maybe/c function/c)
                      #:side (one-of/c 'left 'right 'both)
                      #:how-many (maybe/c exact-nonnegative-integer?))
                     sequence?)]
          [trim-by (map/c (head exact-nonnegative-integer?
                                exact-nonnegative-integer?))]
          [index-of (->* (any/c sequence?)
                         (#:key (maybe/c function/c))
                         (maybe/c exact-nonnegative-integer?))]
          [index (->* (any/c sequence?)
                      (#:key (maybe/c function/c))
                      (maybe/c exact-nonnegative-integer?))]
          [remove (->* (any/c sequence?)
                       (#:key (maybe/c function/c)
                        #:how-many (maybe/c exact-nonnegative-integer?))
                       sequence?)]
          [remove-at (map/c (head natural-number/c))]
          [drop-when (->* (predicate/c sequence?)
                          (#:how-many (maybe/c exact-nonnegative-integer?))
                          sequence?)]
          [intersperse (map/c (head any/c))]
          [add-between (map/c (head any/c))]
          [join-with (binary-function/c any/c
                                        sequence?
                                        any/c)] ; parametrize the type here?
          [wrap-each (map/c (head any/c any/c))]
          [weave (binary-operation/c any/c
                                     (tail sequence?))]
          [interleave (variadic-composition/c sequence? (head sequence?))]))

(define take-when filter)

(define prefix take)

(define suffix-at drop)

(define (init seq)
  (match seq
    [(sequence v) empty-stream]
    [(sequence v vs ...) (stream-cons v (init vs))]))

(define (suffix n seq)
  (drop (- (length seq) n) seq))

(define (infix-at start end seq)
  (subsequence seq start end))

(define (infix start len seq)
  (subsequence* seq start len))

(define (by cnt seq)
  (if (empty? seq)
      empty-stream
      (let ([head (first seq)]
            [tail (with-handlers
                    ([exn:fail:contract?
                      (λ (exn)
                        empty-stream)])
                    (drop cnt seq))])
        (stream-cons head (by cnt tail)))))

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
  (and (not (empty? seq))
       (empty? (rest seq))))

(define (find pred . seqs)
  (let ([vs (take-when (curry apply pred)
                       (apply zip seqs))])
    (if (empty? vs)
        #f
        (let ([result (first vs)])
          (if (singleton? result)
              (join result) ; collapse list for single input sequence
              result)))))

(define (index-where pred . seqs)
  (let loop ([seq (apply zip seqs)]
             [idx 0])
    (match seq
      [(sequence) #f]
      [(sequence v vs ...)
       (if (apply pred v)
           idx
           (loop vs (add1 idx)))])))

(define (choose pred . seqs)
  (map (curry find pred) seqs))

(define (take-while pred seq)
  (match seq
    [(sequence) empty-stream]
    [(sequence v vs ...)
     (if (pred v)
         (stream-cons v (take-while pred vs))
         empty-stream)]))

(define (drop-while pred seq)
  (match seq
    [(sequence) empty-stream]
    [(sequence v vs ...)
     (if (pred v)
         (drop-while pred vs)
         seq)]))

(define (take-until pred seq)
  (take-while (!! pred) seq))

(define (drop-until pred seq)
  (drop-while (!! pred) seq))

(define (~cut-when pred seq)
  (if (empty? seq)
      (stream ID)
      (let-values ([(chunk remaining) (cut-where pred seq)])
        (match remaining
          [(sequence) (stream-cons chunk empty-stream)]
          [(sequence _ vs ...)
           (stream-cons chunk
                        (~cut-when pred
                                   vs))]))))

(define (cut-when #:trim? [trim? #t]
                  pred
                  seq)
  (~cut-when pred
             (if trim?
                 (trim-if pred seq)
                 seq)))

(define (cut #:key [key #f]
             #:trim? [trim? #t]
             elem
             seq)
  (cut-when #:trim? trim?
            (curry = #:key key elem)
            seq))

(define (cut-at pos seq)
  (let ([left (take pos seq)]
        [right (drop pos seq)])
    (values left right)))

(define (cut-where pred seq)
  (let ([left (take-until pred seq)]
        [right (drop-until pred seq)])
    (values left right)))

(define (cut-by n seq)
  (by n (infixes n seq)))

(define (cut-with pred seq)
  (values (take-when pred seq)
          (drop-when pred seq)))

(define (rotate-left n seq)
  (if (empty? seq)
      seq
      (with-handlers ([exn:fail:contract?
                       (λ (exn)
                         (set! n (remainder n (length seq)))
                         (d:append (drop n seq)
                                   (take n seq)))])
        ;; attempt it lazily first; only rotate modulo length
        ;; if it fails, since computing length is not lazy
        (d:append (drop n seq)
                  (take n seq)))))

(define rotate (curry rotate-left 1))

(define (rotate-right n seq)
  ;; this operation must compute sequence length and so it isn't lazy
  (if (empty? seq)
      seq
      (let* ([len (length seq)]
             [n (remainder n len)])
        (d:append (drop (- len n) seq)
                  (take (- len n) seq)))))

(define (truncate seq ref-seq)
  (zip-with (arg 0) seq ref-seq))

(define (rotations seq)
  ;; adapted from a comment on:
  ;; https://stackoverflow.com/a/43507769/323874
  (truncate (iterate rotate seq)
            seq))

(define (deduplicate seq #:key [key #f])
  (->list
   (apply generic-set
          #:key key
          seq)))

(define (interleave . seqs)
  (if (empty? seqs)
      empty-stream
      (let loop ([remaining-seqs seqs])
        (match remaining-seqs
          [(sequence) (apply interleave (map rest seqs))]
          [(sequence (sequence) _ ...) empty-stream]
          [(sequence seq seqs ...)
           (stream-cons (first seq)
                        (loop seqs))]))))

(define (suffixes seq)
  (if (empty? seq)
      (stream empty-stream)
      (stream-cons seq (suffixes (rest seq)))))

(define (prefixes seq [n 0])
  (if (empty? seq)
      (stream empty-stream)
      (let ([pfx (with-handlers ([exn:fail:contract? false.])
                   (prefix n seq))])
        (if pfx
            (stream-cons pfx (prefixes seq (add1 n)))
            empty-stream))))

(define (infixes len seq)
  (if (empty? seq)
      empty-stream
      (let ([infix (with-handlers ([exn:fail:contract? false.])
                     ;; convert to list or the exception would
                     ;; be deferred here
                     (->list (prefix len seq)))])
        (if infix
            (stream-cons infix (infixes len (rest seq)))
            empty-stream))))

(define (prefix? #:key [key #f] prefix seq)
  (cond [(empty? prefix) #t]
        [(empty? seq) #f]
        [else (and (= #:key key
                      (first seq)
                      (first prefix))
                   (prefix? #:key key
                            (rest prefix)
                            (rest seq)))]))

(define starts-with? prefix?)

(define (suffix? #:key [key #f] suffix seq)
  (prefix? #:key key
           (reverse suffix)
           (reverse seq)))

(define ends-with? suffix?)

(define (find-infix #:key [key #f] subseq seq [idx 0])
  (match* (seq subseq)
    [(_ (sequence)) 0]
    [((sequence) _) #f]
    [((sequence v remaining-seq ...) (sequence w remaining-subseq ...))
     (if (and (= #:key key v w)
              (prefix? #:key key
                       remaining-subseq
                       remaining-seq))
         idx
         (find-infix #:key key
                     subseq
                     remaining-seq
                     (add1 idx)))]))

(define (~remain? how-many)
  (or (not how-many)
      (> how-many 0)))

(define (replace-infix #:key [key #f]
                       #:how-many [how-many #f]
                       orig-subseq
                       new-subseq
                       seq)
  (let ([found-index (find-infix #:key key
                                 orig-subseq
                                 seq)])
    (if (and (~remain? how-many)
             found-index)
        (.. (take found-index seq)
            (->stream new-subseq)
            (replace-infix #:key key
                           orig-subseq
                           new-subseq
                           (drop (+ found-index
                                    (length orig-subseq))
                                 seq)
                           #:how-many (and how-many (sub1 how-many))))
        seq)))

(define (infix? #:key [key #f] subseq seq)
  (->boolean (find-infix #:key key subseq seq)))

(define contains? infix?)

(define (trim-left-if pred
                      seq
                      #:how-many [how-many #f])
  (match seq
    [(sequence) seq]
    [(sequence v vs ...)
     (if (and (~remain? how-many)
              (pred v))
         (trim-left-if pred
                       vs
                       #:how-many (and how-many
                                       (sub1 how-many)))
         seq)]))

(define (trim-right-if pred
                       seq
                       #:how-many [how-many #f])
  (reverse (trim-left-if pred
                         (reverse seq)
                         #:how-many how-many)))

(define (trim-if pred
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

(define (trim elem
              seq
              #:key [key #f]
              #:side [side 'both]
              #:how-many [how-many #f])
  (trim-if (curry = #:key key elem)
           seq
           #:side side
           #:how-many how-many))

(define (trim-by left right seq)
  (let ([len (length seq)])
    (when (> (+ left right)
             len)
        (raise-arguments-error 'trim-by
                               "Trimmed lengths exceed total length!"
                               "left" left
                               "right" right
                               "sequence length" len))
    (take (- len
             (+ left
                right))
          (drop left seq))))

(define (index-of #:key [key #f]
                  elem
                  seq)
  (d:index-of seq
              elem
              (curry = #:key key)))

(define index index-of)

(define (drop-when #:how-many [how-many #f]
                   pred
                   seq)
  (when ((|| set? gset?) seq)
    (raise-argument-error 'drop-when
                          "sequence? that is not a pure set"
                          seq))
  (match seq
    [(sequence) seq]
    [(sequence v vs ...)
     (if how-many
         (if (> how-many 0)
             (if (pred v)
                 (drop-when #:how-many (sub1 how-many)
                            pred
                            vs)
                 (stream-cons v
                              (drop-when #:how-many how-many
                                         pred
                                         vs)))
             seq)
         (take-when (!! pred) seq))]))

(define (remove-at pos seq)
  (.. (take pos seq)
      (drop (add1 pos) seq)))

(define (remove #:key [key #f]
                #:how-many [how-many #f]
                elem
                seq)
  (if ((|| set? gset?) seq)
      (set-remove seq elem)
      (drop-when #:how-many how-many
                 (curry = #:key key elem)
                 seq)))

(define (intersperse sep seq)
  (match seq
    [(or (sequence) (sequence _)) seq]
    [(sequence v vs ...)
     (stream-cons v
                  (stream-cons sep
                               (intersperse sep vs)))]))

(define add-between intersperse)

(define (wrap-each before after seq)
  (match seq
    [(sequence) seq]
    [(sequence v vs ...)
     (let ([wrapped-v (stream before v after)])
       (if (empty? vs)
           wrapped-v
           (stream-append wrapped-v
                          (wrap-each before after vs))))]))

(define join-with
  (.. join intersperse))

(define weave
  (.. join wrap-each))

(define (multiples elem [n 0])
  (map (curry * elem) (naturals n)))

(define (powers elem [op ..])
  (map (curryr (curry power elem) op)
       (naturals)))

(define (iterate op elem)
  (unfold (sequencer values op) elem))
