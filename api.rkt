#lang racket/base

(require (except-in data/collection
                    range)
         syntax/parse/define
         (for-syntax racket/base)
         arguments
         (prefix-in p: "base.rkt")
         "types.rkt")

;; TODO: confirm line count at the end
;; TODO: organize the interfaces - maybe follow the docs order?
(provide
 range
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
 (rename-out [p:exists exists]
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

(define-syntax-parser annotate
  [(_ intf (~optional position:number #:defaults ([position #'0])))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (if (and (known-finite? seq) (not (known-finite? result)))
             (finite-sequence result)
             result)))]
  [(_ intf (~datum LIST))
   #'(lambda/arguments args
       (let ([seq (first (first (arguments-positional args)))]
             [result (apply/arguments intf args)])
         (if (and (known-finite? seq) (not (known-finite? result)))
             (finite-sequence result)
             result)))]
  [(_ intf position:number (~datum VALUES))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)])
         (let-values ([(a b) (apply/arguments intf args)])
           (if (and (known-finite? seq)
                    (or (not (known-finite? a))
                        (not (known-finite? b))))
               (values (finite-sequence a)
                       (finite-sequence b))
               (values a b)))))]
  [(_ intf position:number (~datum SEQS))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (if (and (known-finite? seq)
                  (not (empty? result))
                  (not (known-finite? (first result))))
             (map finite-sequence result)
             result)))])

(define (range . args)
  (finite-sequence (apply in-range args)))

(define by (annotate p:by 1))

(define take-when (annotate p:take-when 1))

(define prefix (annotate p:prefix 1))

(define suffix-at (annotate p:suffix-at 1))

(define infix-at (annotate p:infix-at 2))

(define infix (annotate p:infix 2))

(define init (annotate p:init))

(define zip-with (annotate p:zip-with 1))

(define zip (annotate p:zip 0))

(define unzip-with (annotate p:unzip-with LIST))

(define unzip (annotate p:unzip LIST))

(define choose (annotate p:choose 1))

(define suffix (annotate p:suffix 1))

(define take-while (annotate p:take-while 1))

(define drop-while (annotate p:drop-while 1))

(define take-until (annotate p:take-until 1))

(define drop-until (annotate p:drop-until 1))

(define cut-when (annotate p:cut-when 1))

(define cut (annotate p:cut 1))

(define cut-at (annotate p:cut-at 1 VALUES))

(define cut-where (annotate p:cut-where 1 VALUES))

(define cut-by (annotate p:cut-by 1 SEQS))

(define cut-with (annotate p:cut-with 1 VALUES))

(define truncate (annotate p:truncate 1))

(define rotate-left (annotate p:rotate-left 1))

(define rotate-right (annotate p:rotate-right 1))

(define rotate (annotate p:rotate))

(define rotations (annotate p:rotations 0 SEQS))

(define prefixes (annotate p:prefixes 0 SEQS))

(define suffixes (annotate p:suffixes 0 SEQS))

(define infixes (annotate p:infixes 1 SEQS))

;; not sure about this one
(define replace-infix (annotate p:replace-infix 2))

(define trim-if (annotate p:trim-if 1))

(define trim (annotate p:trim 1))

(define trim-by (annotate p:trim-by 2))

(define remove (annotate p:remove 1))

(define remove-at (annotate p:remove-at 1))

(define drop-when (annotate p:drop-when 1))

(define intersperse (annotate p:intersperse 1))

(define add-between (annotate p:add-between 1))

(define wrap-each (annotate p:wrap-each 2))

;; really it's if _any_ of the input sequences is finite
(define interleave (annotate p:interleave 0))
