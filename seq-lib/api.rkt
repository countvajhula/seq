#lang racket/base

(require (only-in racket/function curry)
         (except-in data/collection
                    range
                    map
                    filter
                    reverse
                    rest
                    nth
                    index-where
                    index-of
                    take
                    drop)
         (only-in data/collection
                  [map d:map]
                  [filter d:filter]
                  [reverse d:reverse]
                  [rest d:rest]
                  [take d:take]
                  [drop d:drop])
         syntax/parse/define
         (for-syntax racket/base)
         version-case
         "private/util.rkt"
         (prefix-in p: "base.rkt")
         "types.rkt")

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(provide range
         map
         filter
         reverse
         rest
         drop
         set-nth
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
         (all-from-out data/collection)
         (rename-out [d:take take] ; so scribble can find it in `(for-label seq)`
                     [p:nth nth]
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
                     [p:weave weave])
         ;; these "annotate" utilities are considered internal implementation
         ;; details and are not intended to be used outside this module
         ;; except for testing
         (rename-out [annotate-result seq-test:annotate-result]
                     [annotate-result-naively seq-test:annotate-result-naively]))

(define (annotate-result source result)
  (if (and source
           (countable? source)
           (known-finite? source)
           (not (and (countable? result)
                     (known-finite? result))))
      (finite-sequence result)
      result))

(define (annotate-result-naively result)
  (if (and (countable? result)
           (known-finite? result))
      result
      (finite-sequence result)))

;;; built-in or data/collection sequences
(define (range . args)
  (finite-sequence (apply in-range args)))

;; `define-by-annotating` is used to annotate interfaces where the
;; finiteness of the result is implied by the finiteness of the input(s)
;; In writing these annotated functions, if we accepted an arbitrary
;; number of arguments, it would obscure the arity of the underlying
;; function, confusing currying attempts and possibly having other
;; unintended consequences. Therefore, we need to write the annotated
;; functions here so that they have identical signatures to the
;; underyling functions being annotated.
(define-syntax-parser define-by-annotating
  [(_ fname f 1)
   ;; function taking exactly one argument
   ;; which is the sequence itself
   #'(define (fname seq)
       (let ([result (f seq)])
         (annotate-result seq result)))]
  [(_ fname f 2 1)
   ;; function taking two arguments, with the sequence
   ;; at position 1 (0-indexed)
   #'(define (fname arg seq)
       (let ([result (f arg seq)])
         (annotate-result seq result)))]
  [(_ fname f 2 0)
   ;; function taking two arguments, with the sequence
   ;; at position 0 (0-indexed)
   #'(define (fname seq arg)
       (let ([result (f seq arg)])
         (annotate-result seq result)))]
  [(_ fname f 3 2)
   ;; function taking three arguments, with the sequence
   ;; at position 2 (0-indexed)
   #'(define (fname arg1 arg2 seq)
       (let ([result (f arg1 arg2 seq)])
         (annotate-result seq result)))]
  [(_ fname f (~datum VARIADIC-INPUT))
   ;; function taking any number of sequence arguments
   #'(define (fname . seqs)
       (let ([result (apply f seqs)]
             [seq (first seqs)])
         (annotate-result seq result)))]
  [(_ fname f 1 (~datum VARIADIC-INPUT))
   ;; function taking an arbitrary argument, followed by
   ;; any number of sequence arguments
   #'(define (fname arg . seqs)
       (let ([result (apply f arg seqs)]
             [seq (first seqs)])
         (annotate-result seq result)))]
  [(_ fname f (~datum LIST-INPUT))
   ;; function taking a list of sequences as its sole argument
   #'(define (fname seqs)
       (let ([result (f seqs)]
             [seq (first seqs)])
         (annotate-result seq result)))]
  [(_ fname f 1 (~datum LIST-INPUT))
   ;; function taking an arbitrary argument, followed by
   ;; a list of sequences as its second argument
   #'(define (fname arg seqs)
       (let ([result (f arg seqs)]
             [seq (first seqs)])
         (annotate-result seq result)))]
  [(_ fname f 2 1 (~datum TWO-VALUE-RESULT))
   ;; function taking an arbitrary argument followed by
   ;; a sequence, returning two values, each a sequence
   ;; TODO: what about the wrapping sequence in a sequence of sequences?
   #'(define (fname arg seq)
       (let-values ([(a b) (f arg seq)])
         (values (annotate-result seq a)
                 (annotate-result seq b))))]
  [(_ fname f 1 (~datum SEQUENCE-RESULT))
   ;; function taking a single sequence argument
   ;; that returns a sequence of sequences
   #'(define (fname seq)
       (let ([result (f seq)])
         (d:map (curry annotate-result seq) result)))]
  [(_ fname f 2 1 (~datum SEQUENCE-RESULT))
   ;; function taking a single sequence argument
   ;; that returns a sequence of sequences
   #'(define (fname arg seq)
       (let ([result (f arg seq)])
         (d:map (curry annotate-result seq) result)))])

;; `define-by-annotating-naively` is for interfaces where the result is
;; always going to be finite, irrespective of the input.  this could
;; support additional patterns like `annotate`, if needed, but this is
;; only used in `choose` at the moment
(define-syntax-parser define-by-annotating-naively
  [(_ fname f 1 (~datum VARIADIC-INPUT))
   #'(define (fname arg . seqs)
       (let ([result (apply f arg seqs)])
         (annotate-result-naively result)))])

;; really it's if _any_ of the input sequences are finite
(define-by-annotating map d:map 1 VARIADIC-INPUT)
(define-by-annotating filter d:filter 2 1)
(define-by-annotating reverse d:reverse 1)
(define-by-annotating rest d:rest 1)
(define-by-annotating drop d:drop 2 1)
(define-by-annotating set-nth p:set-nth 3 2)

;;; seq
(define-by-annotating by p:by 2 1)
(define-by-annotating take-when p:take-when 2 1)
(define-by-annotating prefix p:prefix 2 1)

(define-by-annotating suffix-at p:suffix-at 2 1)

(define-by-annotating infix-at p:infix-at 3 2)

(define-by-annotating infix p:infix 3 2)

(define-by-annotating init p:init 1)

;; really it's if _any_ of the input sequences are finite
(define-by-annotating zip-with p:zip-with 1 VARIADIC-INPUT)

(define-by-annotating zip p:zip VARIADIC-INPUT)

(define-by-annotating unzip-with p:unzip-with 1 LIST-INPUT)

(define-by-annotating unzip p:unzip LIST-INPUT)

(define-by-annotating-naively choose p:choose 1 VARIADIC-INPUT)

(define-by-annotating suffix p:suffix 2 1)

(define-by-annotating take-while p:take-while 2 1)

(define-by-annotating drop-while p:drop-while 2 1)

(define-by-annotating take-until p:take-until 2 1)

(define-by-annotating drop-until p:drop-until 2 1)

(define (cut-when pred seq)
  (d:map (curry annotate-result seq)
         (p:cut-when pred seq)))

(define (cut #:key [key #f]
             elem
             seq)
  (d:map (curry annotate-result seq)
         (p:cut #:key key
                elem
                seq)))

(define-by-annotating cut-at p:cut-at 2 1 TWO-VALUE-RESULT)

(define-by-annotating cut-where p:cut-where 2 1 TWO-VALUE-RESULT)

;; cut-by appears to always return a stream of lists, so it can probably
;; just be passed through here as it's guaranteed to be known finite
;; (as a list)
(define-by-annotating cut-by p:cut-by 2 1 SEQUENCE-RESULT)

(define-by-annotating cut-with p:cut-with 2 1 TWO-VALUE-RESULT)

(define-by-annotating truncate p:truncate 2 0)

(define-by-annotating rotate-left p:rotate-left 2 1)

(define-by-annotating rotate-right p:rotate-right 2 1)

(define-by-annotating rotate p:rotate 1)

(define-by-annotating rotations p:rotations 1 SEQUENCE-RESULT)

(define-by-annotating prefixes p:prefixes 1 SEQUENCE-RESULT)

(define-by-annotating suffixes p:suffixes 1 SEQUENCE-RESULT)

(define-by-annotating infixes p:infixes 2 1 SEQUENCE-RESULT)

(define (replace-infix #:key [key #f]
                       #:how-many [how-many #f]
                       orig-subseq
                       new-subseq
                       seq)
  (annotate-result seq
                   (p:replace-infix #:key key
                                    #:how-many how-many
                                    orig-subseq
                                    new-subseq
                                    seq)))

(define (trim-if pred
                 seq
                 #:side [side 'both]
                 #:how-many [how-many #f])
  (annotate-result seq
                   (p:trim-if #:side side
                              #:how-many how-many
                              pred
                              seq)))

(define (trim elem
              seq
              #:key [key #f]
              #:side [side 'both]
              #:how-many [how-many #f])
  (annotate-result seq
                   (p:trim #:key key
                           #:side side
                           #:how-many how-many
                           elem
                           seq)))

(define-by-annotating trim-by p:trim-by 3 2)

(define (remove #:key [key #f]
                #:how-many [how-many #f]
                elem
                seq)
  (annotate-result seq
                   (p:remove #:key key
                             #:how-many how-many
                             elem
                             seq)))

(define-by-annotating remove-at p:remove-at 2 1)

(define (drop-when #:how-many [how-many #f]
                   pred
                   seq)
  (annotate-result seq
                   (p:drop-when #:how-many how-many
                                pred
                                seq)))

(define-by-annotating intersperse p:intersperse 2 1)

(define-by-annotating add-between p:add-between 2 1)

(define-by-annotating wrap-each p:wrap-each 3 2)

;; really it's if _any_ of the input sequences are finite
(define-by-annotating interleave p:interleave VARIADIC-INPUT)
