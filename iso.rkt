#lang racket/base

(require racket/set
         racket/list
         arguments
         (only-in racket/function curry)
         (for-syntax racket/base
                     arguments)
         (only-in data/collection
                  sequence->list
                  apply
                  map
                  known-finite?
                  nth
                  extend
                  collection?)
         relation/type
         (only-in relation
                  false.
                  appendable-identity)
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

;; HERE: also add the char string conversions at this level and remove from base

;; TODO: document that custom types must implement gen:collection with
;; consideration to ordering, and also gen:appendable
(define (return source result)
  (cond [(and source (list? source) (known-finite? result)) (->list result)]
        [(and source (string? source) (known-finite? result)) (->string result)]
        [(and source (vector? source) (known-finite? result)) (->vector result)]
        [(and source (bytes? source) (known-finite? result)) (->bytes result)]
        [(and source (set? source) (known-finite? result)) (->set result)]
        [(and source (hash? source) (known-finite? result)) (->hash result)]
        [(and source (collection? source) (known-finite? result))
         (let ([null-cons (appendable-identity source)])
           (extend null-cons result))]
        [else result]))

(define-syntax-parser iso
  [(_ intf (~optional position:number #:defaults ([position #'0])))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (return seq result)))]
  [(_ intf (~optional position:number #:defaults ([position #'0])) (~datum VARIADIC))
   #'(lambda/arguments args
       (let ([seq (with-handlers ([exn:fail? false.])
                    (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (return seq result)))]
  [(_ intf position:number (~datum LIST))
   #'(lambda/arguments args
       (let ([seq (first (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (return seq result)))]
  [(_ intf position:number (~datum VALUES))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)])
         (let-values ([(a b) (apply/arguments intf args)])
           (values (return seq a)
                   (return seq b)))))]
  [(_ intf position:number (~datum SEQS))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (map (curry return seq) result)))])

(define by (iso p:by 1))

(define take-when (iso p:take-when 1))

(define prefix (iso p:prefix 1))

(define suffix-at (iso p:suffix-at 1))

(define infix-at (iso p:infix-at 2))

(define infix (iso p:infix 2))

(define init (iso p:init))

(define zip-with (iso p:zip-with 1))

(define zip (iso p:zip 0))

(define unzip-with (iso p:unzip-with 1 LIST))

(define unzip (iso p:unzip 0 LIST))

(define choose (iso p:choose 1 VARIADIC))

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
