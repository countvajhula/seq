#lang racket/base

(require racket/set
         racket/list
         arguments
         (for-syntax racket/base
                     arguments)
         (only-in data/collection
                  sequence->list
                  apply
                  known-finite?
                  nth)
         relation/type
         (prefix-in p: "api.rkt")
         syntax/parse/define)

(provide
 by
 take-when
 prefix
 suffix-at
 infix
 infix-at
 init
 zip-with
 zip
 unzip-with
 unzip
 choose
 suffix
 take-while
 drop-while
 take-until
 drop-until
 cut-when
 cut
 cut-at
 cut-where
 cut-by
 cut-with
 truncate
 rotate-left
 rotate-right
 rotate
 rotations
 suffixes
 prefixes
 infixes
 replace-infix
 trim-if
 trim
 trim-by
 remove
 remove-at
 drop-when
 intersperse
 add-between
 wrap-each
 interleave
 (rename-out [p:range range]
             [p:exists exists]
             [p:for-all for-all]
             [p:find find]
             [p:index-where index-where]
             [p:deduplicate deduplicate]
             [p:multiples multiples]
             [p:powers powers]
             [p:iterate iterate]
             [p:prefix? prefix?]
             [p:starts-with? starts-with?]
             [p:suffix? suffix?]
             [p:ends-with? ends-with?]
             [p:find-infix find-infix]
             [p:infix? infix?]
             [p:contains? contains?]
             [p:index-of index-of]
             [p:index index]
             [p:join-with join-with]
             [p:weave weave]))

(define-syntax-parser iso
  [(_ intf (~optional position:number #:defaults ([position #'0])))
   #'(lambda/arguments args
                       (let ([seq (nth (arguments-positional args) position)]
                             [result (apply/arguments intf args)])
                         (cond [(and seq (list? seq) (known-finite? result)) (->list result)]
                               [(and seq (string? seq) (known-finite? result)) (->string result)]
                               [(and seq (vector? seq) (known-finite? result)) (->vector result)]
                               [(and seq (bytes? seq) (known-finite? result)) (->bytes result)]
                               [(and seq (set? seq) (known-finite? result)) (->set result)]
                               [(and seq (hash? seq) (known-finite? result)) (->hash result)]
                               [else result])))]
  [(_ intf (~datum LIST))
   #'(lambda/arguments args
                       (let ([seq (first (first (arguments-positional args)))]
                             [result (apply/arguments intf args)])
                         (cond [(and seq (list? seq) (known-finite? result)) (->list result)]
                               [(and seq (string? seq) (known-finite? result)) (->string result)]
                               [(and seq (vector? seq) (known-finite? result)) (->vector result)]
                               [(and seq (bytes? seq) (known-finite? result)) (->bytes result)]
                               [(and seq (set? seq) (known-finite? result)) (->set result)]
                               [(and seq (hash? seq) (known-finite? result)) (->hash result)]
                               [else result])))]
  [(_ intf position:number (~datum VALUES))
   #'(lambda/arguments args
                       (let ([seq (nth (arguments-positional args) position)])
                         (let-values ([(a b) (apply/arguments intf args)])
                           (cond [(and seq (list? seq) (known-finite? a) (known-finite? b))
                                  (values (->list a) (->list b))]
                                 [(and seq (string? seq) (known-finite? a) (known-finite? b))
                                  (values (->string a) (->string b))]
                                 [(and seq (vector? seq) (known-finite? a) (known-finite? b))
                                  (values (->vector a) (->vector b))]
                                 [(and seq (bytes? seq) (known-finite? a) (known-finite? b))
                                  (values (->bytes a) (->bytes b))]
                                 [(and seq (set? seq) (known-finite? a) (known-finite? b))
                                  (values (->set a) (->set b))]
                                 [(and seq (hash? seq) (known-finite? a) (known-finite? b))
                                  (values (->hash a) (->hash b))]
                                 [else (values a b)]))))]
  [(_ intf position:number (~datum SEQS))
   #'(lambda/arguments args
                       (let ([seq (nth (arguments-positional args) position)]
                             [result (apply/arguments intf args)])
                         (cond [(and seq (list? seq) (known-finite? result)) (map ->list result)]
                               [(and seq (string? seq) (known-finite? result)) (map ->string result)]
                               [(and seq (vector? seq) (known-finite? result)) (map ->vector result)]
                               [(and seq (bytes? seq) (known-finite? result)) (map ->bytes result)]
                               [(and seq (set? seq) (known-finite? result)) (map ->set result)]
                               [(and seq (hash? seq) (known-finite? result)) (map ->hash result)]
                               [else result])))])

(define by (iso p:by 1))

(define take-when (iso p:take-when 1))

(define prefix (iso p:prefix 1))

(define suffix-at (iso p:suffix-at 1))

(define infix-at (iso p:infix-at 2))

(define infix (iso p:infix 2))

(define init (iso p:init))

(define zip-with (iso p:zip-with 1))

(define zip (iso p:zip 0))

(define unzip-with (iso p:unzip-with LIST))

(define unzip (iso p:unzip LIST))

(define choose (iso p:choose 1))

(define suffix (iso p:suffix 1))

(define take-while (iso p:take-while 1))

(define drop-while (iso p:drop-while 1))

(define take-until (iso p:take-until 1))

(define drop-until (iso p:drop-until 1))

(define cut-when (iso p:cut-when 1))

(define cut (iso p:cut 1))

(define cut-at (iso p:cut-at 1 VALUES))

(define cut-where (iso p:cut-where 1 VALUES))

(define cut-by (iso p:cut-by 1 SEQS))

(define cut-with (iso p:cut-with 1 VALUES))

(define truncate (iso p:truncate 1))

(define rotate-left (iso p:rotate-left 1))

(define rotate-right (iso p:rotate-right 1))

(define rotate (iso p:rotate))

(define rotations (iso p:rotations 0 SEQS))

(define prefixes (iso p:prefixes 0 SEQS))

(define suffixes (iso p:suffixes 0 SEQS))

(define infixes (iso p:infixes 1 SEQS))

;; not sure about this one
(define replace-infix (iso p:replace-infix 2))

(define trim-if (iso p:trim-if 1))

(define trim (iso p:trim 1))

(define trim-by (iso p:trim-by 2))

(define remove (iso p:remove 1))

(define remove-at (iso p:remove-at 1))

(define drop-when (iso p:drop-when 1))

(define intersperse (iso p:intersperse 1))

(define add-between (iso p:add-between 1))

(define wrap-each (iso p:wrap-each 2))

;; really it's if _any_ of the input sequences is finite
(define interleave (iso p:interleave 0))
