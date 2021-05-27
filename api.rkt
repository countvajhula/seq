#lang racket/base

(require (except-in data/collection
                    range)
         syntax/parse/define
         (for-syntax racket/base)
         arguments
         (prefix-in p: "base.rkt")
         "types.rkt")

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
 (rename-out [p:cut-by cut-by]
             [p:cut-with cut-with]
             [p:truncate truncate]
             [p:rotate-left rotate-left]
             [p:rotate-right rotate-right]
             [p:rotate rotate]
             [p:rotations rotations]
             [p:deduplicate deduplicate]
             [p:multiples multiples]
             [p:powers powers]
             [p:iterate iterate]
             [p:suffixes suffixes]
             [p:prefixes prefixes]
             [p:infixes infixes]
             [p:prefix? prefix?]
             [p:starts-with? starts-with?]
             [p:suffix? suffix?]
             [p:ends-with? ends-with?]
             [p:find-infix find-infix]
             [p:replace-infix replace-infix]
             [p:infix? infix?]
             [p:contains? contains?]
             [p:trim-if trim-if]
             [p:trim trim]
             [p:trim-by trim-by]
             [p:index-of index-of]
             [p:index index]
             [p:remove remove]
             [p:remove-at remove-at]
             [p:drop-when drop-when]
             [p:intersperse intersperse]
             [p:add-between add-between]
             [p:join-with join-with]
             [p:wrap-each wrap-each]
             [p:weave weave]
             [p:interleave interleave]

             [p:exists exists]
             [p:for-all for-all]
             [p:find find]
             [p:index-where index-where]))

(define-syntax-parser annotate
  [(_ intf (~optional position:number #:defaults ([position #'0])))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (if (known-finite? seq)
             (finite-sequence result)
             result)))]
  [(_ intf (~datum LIST))
   #'(lambda/arguments args
       (let ([seq (first (first (arguments-positional args)))]
             [result (apply/arguments intf args)])
         (if (known-finite? seq)
             (finite-sequence result)
             result)))]
  [(_ intf position:number (~datum VALUES))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)])
         (let-values ([(a b) (apply/arguments intf args)])
           (if (known-finite? seq)
               (values (finite-sequence a)
                       (finite-sequence b))))))])

(define/arguments (range args)
  ;; TODO: submit this fix to data/collection
  (finite-sequence (apply/arguments in-range args)))

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
