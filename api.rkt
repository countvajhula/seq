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
 exists
 for-all
 init
 zip-with
 zip
 (rename-out [p:unzip-with unzip-with]
             [p:unzip unzip]
             [p:find find]
             [p:index-where index-where]
             [p:choose choose]
             [p:suffix suffix]
             [p:take-while take-while]
             [p:drop-while drop-while]
             [p:take-until take-until]
             [p:drop-until drop-until]
             [p:cut-when cut-when]
             [p:cut cut]
             [p:cut-at cut-at]
             [p:cut-where cut-where]
             [p:cut-by cut-by]
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
             [p:interleave interleave]))

(define-syntax-parser annotate
  [(_ intf)
   #'(lambda/arguments args
       (let* ([pos-args (arguments-positional args)]
              [seq (first pos-args)])
         (let ([result (intf seq)])
           (if (known-finite? seq)
               (finite-sequence result)
               result))))]
  [(_ intf (~datum AO))
   #'(lambda/arguments args
       (let* ([pos-args (arguments-positional args)]
              [arg (first pos-args)]
              [seq (second pos-args)])
         (let ([result (intf arg seq)])
           (if (known-finite? seq)
               (finite-sequence result)
               result))))]
  [(_ intf (~datum AAO))
   #'(lambda/arguments args
       (let* ([pos-args (arguments-positional args)]
              [arg1 (first pos-args)]
              [arg2 (second pos-args)]
              [seq (third pos-args)])
         (let ([result (intf arg1 arg2 seq)])
           (if (known-finite? seq)
               (finite-sequence result)
               result))))]
  [(_ intf (~datum A-OS))
   #'(lambda/arguments args
       (let* ([pos-args (arguments-positional args)]
              [arg (first pos-args)]
              [seqs (rest pos-args)]
              [seq (and (not (empty? seqs)) (first seqs))])
         (let ([result (apply intf arg seqs)])
           (if (known-finite? seq)
               (finite-sequence result)
               result))))]
  [(_ intf (~datum -OS))
   #'(lambda/arguments args
       (let* ([seqs (arguments-positional args)]
              [seq (and (not (empty? seqs)) (first seqs))])
         (let ([result (apply intf seqs)])
           (if (known-finite? seq)
               (finite-sequence result)
               result))))])

(define/arguments (range args)
  ;; TODO: submit this fix to data/collection
  (finite-sequence (apply/arguments in-range args)))

(define by (annotate p:by AO))

(define take-when (annotate p:take-when AO))

(define prefix (annotate p:prefix AO))

(define suffix-at (annotate p:suffix-at AO))

(define infix-at (annotate p:infix-at AAO))

(define infix (annotate p:infix AAO))

(define exists (annotate p:exists A-OS))

(define for-all (annotate p:for-all A-OS))

(define init (annotate p:init))

(define zip-with (annotate p:zip-with A-OS))

(define zip (annotate p:zip -OS))