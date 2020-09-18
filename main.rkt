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

(provide take-when
         prefix
         suffix-at
         infix
         infix-at
         (contract-out
          [by (-> exact-positive-integer? sequence? sequence?)]
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
          [suffix (-> exact-nonnegative-integer?
                      sequence?
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
          [cut-when (->* ((-> any/c boolean?) sequence?)
                         (#:trim? boolean?)
                         (sequenceof sequence?))]
          [cut (->* (any/c
                     sequence?)
                    (#:key (or/c (-> comparable? comparable?)
                                 #f)
                     #:trim? boolean?)
                    (sequenceof sequence?))]
          [cut-at (-> exact-positive-integer?
                      sequence?
                      (values sequence? sequence?))]
          [cut-where (-> (-> any/c boolean?)
                         sequence?
                         (values sequence? sequence?))]
          [cut-by (-> exact-positive-integer?
                      sequence?
                      (sequenceof sequence?))]
          [cut-with (-> (-> any/c boolean?)
                        sequence?
                        (values sequence? sequence?))]
          [rotate-left (-> exact-nonnegative-integer?
                           sequence?
                           sequence?)]
          [rotate-right (-> exact-nonnegative-integer?
                            sequence?
                            sequence?)]
          [deduplicate (->* (sequence?)
                            (#:key (or/c (-> comparable? comparable?)
                                         #f))
                            list?)]
          [multiples (->* (number?)
                          (natural-number/c)
                          sequence?)]
          [powers (->* (any/c)
                       (procedure?)
                       sequence?)]
          [suffixes (-> sequence?
                        (sequenceof sequence?))]
          [prefixes (-> sequence?
                        (sequenceof sequence?))]
          [infixes (-> exact-positive-integer?
                       sequence?
                       (sequenceof sequence?))]
          [prefix? (->* (sequence? sequence?)
                        (#:key (or/c (-> comparable? comparable?)
                                     #f))
                        boolean?)]
          [starts-with? (->* (sequence? sequence?)
                             (#:key (or/c (-> comparable? comparable?)
                                          #f))
                             boolean?)]
          [suffix? (->* (sequence? sequence?)
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
                               #:how-many exact-nonnegative-integer?)
                              sequence?)]
          [infix? (->* (sequence? sequence?)
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
                         #:how-many (or/c exact-nonnegative-integer?
                                          #f))
                        sequence?)]
          [trim (->* (any/c
                      sequence?)
                     (#:key (or/c (-> comparable? comparable?)
                                  #f)
                      #:side (one-of/c 'left
                                       'right
                                       'both)
                      #:how-many (or/c exact-nonnegative-integer?
                                       #f))
                     sequence?)]
          [trim-by (-> exact-nonnegative-integer?
                       exact-nonnegative-integer?
                       sequence?
                       sequence?)]
          [index-of (->* (any/c sequence?)
                         (#:key (or/c (-> comparable? comparable?)
                                      #f))
                         (or/c exact-nonnegative-integer?
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
                        #:how-many (or/c exact-nonnegative-integer?
                                         #f))
                       sequence?)]
          [drop-when (->* ((-> any/c boolean?)
                           sequence?)
                          (#:how-many (or/c exact-nonnegative-integer?
                                            #f))
                          sequence?)]
          [intersperse (-> any/c
                           sequence?
                           sequence?)]
          [add-between (-> any/c
                           sequence?
                           sequence?)]
          [join-with (-> any/c
                         sequence?
                         any/c)] ; parametrize the type here?
          [wrap-each (-> any/c
                         any/c
                         sequence?
                         sequence?)]
          [weave (-> any/c any/c sequence?
                     (or/c sequence?
                           procedure?))] ; procedure doesn't implement sequence
          [interleave (-> sequence? sequence? ... sequence?)]
          [: (case->
              (any/c any/c . -> . (or/c collection? pair?))
              (any/c #:rest list? . -> . (or/c collection? pair?)))]))

(define :
  (case-lambda
    [(elem col)
     (if (collection? col)
         (conj col elem)
         (cons elem col))]
    [args
     (apply conj* (reverse args))]))

(define take-when filter)

(define prefix take)

(define suffix-at drop)

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
  (let ([result (~cut-when pred
                           (if trim?
                               (trim-if pred seq)
                               seq))])
    (if (string? seq)
        (map ->string result)
        result)))

(define (cut #:key [key #f]
             #:trim? [trim? #t]
             elem
             seq)
  (let ([elem (if (string? seq)
                  (->char elem)
                  elem)])
    (cut-when #:trim? trim?
              (curry = #:key key elem)
              seq)))

(define (cut-at pos seq)
  (let ([left (take pos seq)]
        [right (drop pos seq)])
    (if (string? seq)
        (values (->string left) (->string right))
        (values left right))))

(define (cut-where pred seq)
  (let ([left (take-until pred seq)]
        [right (drop-until pred seq)])
    (if (string? seq)
        (values (->string left) (->string right))
        (values left right))))

(define (cut-by n seq)
  (by n (infixes n seq)))

(define (cut-with pred seq)
  (values (take-when pred seq)
          (drop-when pred seq)))

(define (rotate-left n seq)
  (with-handlers ([exn:fail:contract?
                   (λ (exn)
                     (set! n (remainder n (length seq)))
                     (append (drop n seq)
                             (take n seq)))])
    ;; attempt it lazily first; only rotate modulo length
    ;; if it fails, since computing length is not lazy
    (append (drop n seq)
            (take n seq))))

(define (rotate-right n seq)
  ;; this operation must compute sequence length and so it isn't lazy
  (let* ([len (length seq)]
         [n (remainder n len)])
    (append (drop (- len n) seq)
            (take (- len n) seq))))

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
      empty-stream
      (stream-cons seq (suffixes (rest seq)))))

(define (prefixes seq)
  (define len (and (known-finite? seq) (length seq)))
  (let loop ([n 1])
    (cond [(empty? seq) empty-stream]
          [(and len (> n len)) empty-stream]
          [else (stream-cons (take n seq)
                             (loop (add1 n)))])))

(define (infixes len seq)
  (if (empty? seq)
      empty-stream
      (let ([infix (with-handlers ([exn:fail:contract?
                                    (λ (exn)
                                      empty-stream)])
                     ;; convert to list or the exception would
                     ;; be deferred here
                     (->list (prefix len seq)))])
        (if (empty? infix)
            empty-stream
            (stream-cons infix (infixes len (rest seq)))))))

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

(define (~replace-infix #:key [key #f]
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
            new-subseq
            (~replace-infix #:key key
                            orig-subseq
                            new-subseq
                            (drop (+ found-index
                                     (length orig-subseq))
                                  seq)
                            #:how-many (and how-many (sub1 how-many))))
        seq)))

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
      (match seq
        [(sequence) seq]
        [(sequence v vs ...)
         (if how-many
             (if (> how-many 0)
                 (if (pred v)
                     (~drop-when #:how-many (sub1 how-many)
                                 pred
                                 vs)
                     (stream-cons v
                                  (~drop-when #:how-many how-many
                                              pred
                                              vs)))
                 seq)
             (take-when (!! pred) seq))])))

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

(define (join-with sep seq)
  (join (intersperse sep seq)))

(define (weave to from seq)
  (join (wrap-each to from seq)))

(define (multiples elem [n 0])
  (map (curry * elem) (naturals n)))

(define (powers elem [op ..])
  (map (curryr (curry power elem) op)
       (naturals)))
