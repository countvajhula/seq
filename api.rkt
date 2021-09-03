#lang racket/base

(require (only-in racket/function curry)
         (except-in data/collection
                    range
                    map
                    filter
                    reverse
                    rest
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
         arguments
         (only-in relation false.)
         (prefix-in p: "base.rkt")
         "types.rkt")

(module+ test
  (require rackunit
           rackunit/text-ui
           "private/util.rkt"))

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
                     [p:weave weave]))

(define (annotate-result source result)
  (if (and source
           (countable? source)
           (known-finite? source)
           (not (and (countable? result)
                     (known-finite? result))))
      (finite-sequence result)
      result))

;; annotate the result with whether it is known to be finite,
;; conditioned on the finiteness of the input
;; use this in interfaces where the finiteness of the result
;; is implied by the finiteness of the input(s)
(define-syntax-parser annotate
  [(_ intf (~optional position:number #:defaults ([position #'0])))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (annotate-result seq result)))]
  [(_ intf position:number (~datum VARIADIC-INPUT))
   #'(lambda/arguments args
       (let ([seq (with-handlers ([exn:fail? false.])
                    (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (annotate-result seq result)))]
  [(_ intf position:number (~datum LIST-INPUT))
   #'(lambda/arguments args
       (let ([seq (first (nth (arguments-positional args) position))]
             [result (apply/arguments intf args)])
         (annotate-result seq result)))]
  [(_ intf position:number (~datum TWO-VALUE-RESULT))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)])
         (let-values ([(a b) (apply/arguments intf args)])
           (values (annotate-result seq a)
                   (annotate-result seq b)))))]
  [(_ intf position:number (~datum SEQUENCE-RESULT))
   #'(lambda/arguments args
       (let ([seq (nth (arguments-positional args) position)]
             [result (apply/arguments intf args)])
         (d:map (curry annotate-result seq) result)))])

;; always annotate result, indepdently of input.
;; use this for interfaces where the result is always going
;; to be finite, irrespective of the input.
;; this could support additional patterns like `annotate`,
;; if needed, but this is only used in `choose` at the moment
(define-syntax-parser annotate-naively
  [(_ intf)
   #'(lambda/arguments args
       (let ([result (apply/arguments intf args)])
         (if (and (countable? result)
                  (known-finite? result))
             result
             (finite-sequence result))))])

(module+ test

  ;; the main test module for this is tests/api.rkt
  ;; but we use a test submodule here to avoid providing
  ;; the `annotate` macro outside this module since it's an
  ;; internal implementation detail

  (struct opaque-sequence ()
    #:transparent
    #:methods gen:sequence
    [(define (first this)
       (void))
     (define (rest this)
       (opaque-sequence))
     (define (empty? this)
       #f)]
    #:methods gen:countable
    [(define (known-finite? this)
       #f)
     (define (length this)
       1)])

  (struct known-finite-sequence ()
    #:transparent
    #:methods gen:sequence
    [(define (first this)
       (void))
     (define (rest this)
       (known-finite-sequence))
     (define (empty? this)
       #f)]
    #:methods gen:countable
    [(define (known-finite? this)
       #t)
     (define (length this)
       1)])

  (define tests
    (test-suite
     "finiteness annotation"

     (test-suite
      "annotate conditionally"
      (test-case
          "no position indicated"
        (define (g seq)
          (opaque-sequence))
        (check-true (known-finite? ((annotate g) (known-finite-sequence))))
        (check-false (known-finite? ((annotate g) (opaque-sequence)))))
      (test-case
          "position indicated"
        (define (g elem seq)
          (opaque-sequence))
        (check-true (known-finite? ((annotate g 1) 3 (known-finite-sequence))))
        (check-false (known-finite? ((annotate g 1) 3 (opaque-sequence)))))
      (test-case
          "variadic input"
        (define (g elem . seqs)
          (opaque-sequence))
        (check-true (known-finite? ((annotate g 1 VARIADIC-INPUT) 3
                                                                  (known-finite-sequence)
                                                                  (known-finite-sequence))))
        (check-false (known-finite? ((annotate g 1 VARIADIC-INPUT) 3
                                                                   (opaque-sequence)
                                                                   (opaque-sequence)))))
      (test-case
          "list input"
        (define (g elem seqs)
          (opaque-sequence))
        (check-true
         (known-finite? ((annotate g 1 LIST-INPUT) 3
                                                   (list (known-finite-sequence)
                                                         (known-finite-sequence)))))
        (check-false
         (known-finite? ((annotate g 1 LIST-INPUT) 3
                                                   (list (opaque-sequence)
                                                         (opaque-sequence))))))
      (test-case
          "two value result"
        (define (g elem seq)
          (values (opaque-sequence) (opaque-sequence)))
        (let-values ([(a b) ((annotate g 1 TWO-VALUE-RESULT) 3 (known-finite-sequence))])
          (check-true (known-finite? a))
          (check-true (known-finite? b)))
        (let-values ([(a b) ((annotate g 1 TWO-VALUE-RESULT) 3 (opaque-sequence))])
          (check-false (known-finite? a))
          (check-false (known-finite? b))))
      (test-case
          "sequence result"
        (define (g elem seq)
          (list (opaque-sequence) (opaque-sequence)))
        (check-true (andmap known-finite? ((annotate g 1 SEQUENCE-RESULT) 3 (known-finite-sequence))))
        (check-false (andmap known-finite? ((annotate g 1 SEQUENCE-RESULT) 3 (opaque-sequence))))))

     (test-suite
      "annotate-naively"
      (test-case
          "no position indicated"
        (define (g seq)
          (opaque-sequence))
        (check-true (known-finite? ((annotate-naively g) (known-finite-sequence))))
        (check-true (known-finite? ((annotate-naively g) (opaque-sequence)))))))))

;;; built-in or data/collection sequences
(define (range . args)
  (finite-sequence (apply in-range args)))

;; really it's if _any_ of the input sequences are finite
(define map (annotate d:map 1))

(define filter (annotate d:filter 1))

(define reverse (annotate d:reverse 0))

(define rest (annotate d:rest 0))

(define drop (annotate d:drop 1))

(define set-nth (annotate p:set-nth 2))

;;; seq
(define by (annotate p:by 1))

(define take-when (annotate p:take-when 1))

(define prefix (annotate p:prefix 1))

(define suffix-at (annotate p:suffix-at 1))

(define infix-at (annotate p:infix-at 2))

(define infix (annotate p:infix 2))

(define init (annotate p:init))

;; really it's if _any_ of the input sequences are finite
(define zip-with (annotate p:zip-with 1))

(define zip (annotate p:zip 0))

(define unzip-with (annotate p:unzip-with 1 LIST-INPUT))

(define unzip (annotate p:unzip 0 LIST-INPUT))

(define choose (annotate-naively p:choose))

(define suffix (annotate p:suffix 1))

(define take-while (annotate p:take-while 1))

(define drop-while (annotate p:drop-while 1))

(define take-until (annotate p:take-until 1))

(define drop-until (annotate p:drop-until 1))

(define cut-when (annotate p:cut-when 1 SEQUENCE-RESULT))

(define cut (annotate p:cut 1 SEQUENCE-RESULT))

(define cut-at (annotate p:cut-at 1 TWO-VALUE-RESULT))

(define cut-where (annotate p:cut-where 1 TWO-VALUE-RESULT))

(define cut-by (annotate p:cut-by 1 SEQUENCE-RESULT))

(define cut-with (annotate p:cut-with 1 TWO-VALUE-RESULT))

(define truncate (annotate p:truncate 1))

(define rotate-left (annotate p:rotate-left 1))

(define rotate-right (annotate p:rotate-right 1))

(define rotate (annotate p:rotate))

(define rotations (annotate p:rotations 0 SEQUENCE-RESULT))

(define prefixes (annotate p:prefixes 0 SEQUENCE-RESULT))

(define suffixes (annotate p:suffixes 0 SEQUENCE-RESULT))

(define infixes (annotate p:infixes 1 SEQUENCE-RESULT))

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

;; really it's if _any_ of the input sequences are finite
(define interleave (annotate p:interleave 0))

(module+ test
  (just-do
   (run-tests tests)))
