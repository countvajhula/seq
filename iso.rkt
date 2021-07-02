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
                  set-nth
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
 index-of
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
             [p:index index]
             [p:join-with join-with]
             [p:weave weave]))

(define-syntax-parser string-helper
  [(_ intf)
   #'(string-helper intf 1 0)] ; default to most common sequence and element positions
  [(_ intf seq-position:number elem-position:number)
   #'(lambda/arguments args
       (let* ([pos-args (arguments-positional args)]
              [kw-args (arguments-keyword args)]
              [elem (nth pos-args elem-position)]
              [seq (nth pos-args seq-position)]
              [pos-args (set-nth pos-args elem-position
                                 (if (string? seq)
                                     (->char elem)
                                     elem))]
              [args (make-arguments pos-args kw-args)])
         (apply/arguments intf args)))])

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
  [(_ intf position:number (~datum VARIADIC-INPUT))
   #'(lambda/arguments args
       (let ([seq (with-handlers ([exn:fail? false.])
                    (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (return seq result)))]
  [(_ intf position:number (~datum LIST-INPUT))
   #'(lambda/arguments args
       (let ([seq (first (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (return seq result)))]
  [(_ intf position:number (~datum TWO-VALUE-RESULT))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)])
         (let-values ([(a b) (apply/arguments intf args)])
           (values (return seq a)
                   (return seq b)))))]
  [(_ intf position:number (~datum SEQUENCE-RESULT))
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

(define unzip-with (iso p:unzip-with 1 LIST-INPUT))

(define unzip (iso p:unzip 0 LIST-INPUT))

(define choose (iso p:choose 1 VARIADIC-INPUT))

(define suffix (iso p:suffix 1))

(define take-while (iso p:take-while 1))

(define drop-while (iso p:drop-while 1))

(define take-until (iso p:take-until 1))

(define drop-until (iso p:drop-until 1))

(define cut-when (iso p:cut-when 1 SEQUENCE-RESULT))

(define cut (string-helper (iso p:cut 1 SEQUENCE-RESULT)))

(define cut-at (iso p:cut-at 1 TWO-VALUE-RESULT))

(define cut-where (iso p:cut-where 1 TWO-VALUE-RESULT))

(define cut-by (iso p:cut-by 1 SEQUENCE-RESULT))

(define cut-with (iso p:cut-with 1 TWO-VALUE-RESULT))

(define truncate (iso p:truncate 1))

(define rotate-left (iso p:rotate-left 1))

(define rotate-right (iso p:rotate-right 1))

(define rotate (iso p:rotate))

(define rotations (iso p:rotations 0 SEQUENCE-RESULT))

(define prefixes (iso p:prefixes 0 SEQUENCE-RESULT))

(define suffixes (iso p:suffixes 0 SEQUENCE-RESULT))

(define infixes (iso p:infixes 1 SEQUENCE-RESULT))

;; not sure about this one
(define replace-infix (iso p:replace-infix 2))

(define trim-if (iso p:trim-if 1))

(define trim (string-helper (iso p:trim 1)))

(define trim-by (iso p:trim-by 2))

(define remove (string-helper (iso p:remove 1)))

(define remove-at (iso p:remove-at 1))

(define drop-when (iso p:drop-when 1))

(define intersperse (iso p:intersperse 1))

(define add-between (iso p:add-between 1))

(define wrap-each (iso p:wrap-each 2))

;; really it's if _any_ of the input sequences is finite
(define interleave (iso p:interleave 0))

(define index-of (string-helper p:index-of))
