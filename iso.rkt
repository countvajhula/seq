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
                  andmap
                  known-finite?
                  nth
                  set-nth
                  extend
                  collection?
                  gen:sequence
                  gen:countable)
         relation/type
         (only-in relation
                  false.
                  appendable-identity)
         (prefix-in p: "api.rkt")
         syntax/parse/define)

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/generic
           racket/stream
           "private/util.rkt"))

(provide by
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
         deduplicate
         (rename-out [p:range range]
                     [p:exists exists]
                     [p:for-all for-all]
                     [p:find find]
                     [p:index-where index-where]
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

(module+ test

  ;; the main test module for this is tests/iso.rkt
  ;; but we use a test submodule here to avoid providing
  ;; the `iso` and `string-helper` macros outside this module
  ;; since they're an internal implementation detail

  (struct opaque-sequence (contents)
    #:transparent
    #:methods gen:sequence
    [(define/generic -empty? empty?)
     (define/generic -first first)
     (define/generic -rest rest)
     (define (first this)
       (-first (opaque-sequence-contents this)))
     (define (rest this)
       (-rest (opaque-sequence-contents this)))
     (define (empty? this)
       (-empty? (opaque-sequence-contents this)))]
    #:methods gen:countable
    [(define/generic -length length)
     (define (known-finite? this)
       #f)
     (define (length this)
       (-length (opaque-sequence-contents this)))])

  (struct known-finite-sequence (contents)
    #:transparent
    #:methods gen:sequence
    [(define/generic -empty? empty?)
     (define/generic -first first)
     (define/generic -rest rest)
     (define (first this)
       (-first (known-finite-sequence-contents this)))
     (define (rest this)
       (-rest (known-finite-sequence-contents this)))
     (define (empty? this)
       (-empty? (known-finite-sequence-contents this)))]
    #:methods gen:countable
    [(define/generic -length length)
     (define (known-finite? this)
       #t)
     (define (length this)
       (-length (known-finite-sequence-contents this)))])

  (define tests
    (test-suite
     "isomorphic interfaces"

     (test-suite
      "iso"
      (test-suite
       "basic"
       (test-case
           "known finite result"
         (define (g seq)
           (known-finite-sequence (list 1 2 3)))
         (check-true (list? ((iso g) (list 1))))
         (check-true (vector? ((iso g) #(1))))
         (check-true (known-finite-sequence?
                      ((iso g) (known-finite-sequence (list 1 2 3)))) "custom type")
         (check-true (known-finite-sequence?
                      ((iso g) (opaque-sequence (list 1 2 3))))
                     "output type is unchanged if input is opaque"))
       (test-case
           "opaque result"
         (define (g seq)
           (opaque-sequence (list 1 2 3)))
         (check-false (list? ((iso g) (list 1))))
         (check-false (vector? ((iso g) #(1))))
         (check-false (known-finite-sequence?
                       ((iso g) (known-finite-sequence (list 1 2 3))))
                      "custom type")))
      (test-suite
       "position indicated"
       (test-case
           "known finite result"
         (define (g elem seq)
           (known-finite-sequence (list 1 2 3)))
         (check-true (list? ((iso g 1) 1 (list 1))))
         (check-true (vector? ((iso g 1) 1 #(1))))
         (check-true (known-finite-sequence?
                      ((iso g) 1 (known-finite-sequence (list 1 2 3))))
                     "custom type")
         (check-true (known-finite-sequence?
                      ((iso g) 1 (opaque-sequence (list 1 2 3))))
                     "output type is unchanged if input is opaque"))
       (test-case
           "opaque result"
         (define (g elem seq)
           (opaque-sequence (list 1 2 3)))
         (check-false (list? ((iso g 1) 1 (list 1))))
         (check-false (vector? ((iso g 1) 1 #(1))))
         (check-false (known-finite-sequence?
                       ((iso g) 1 (known-finite-sequence (list 1 2 3))))
                      "custom type")))
      (test-suite
       "variadic input"
       (test-case
           "known finite result"
         (define (g elem . seqs)
           (known-finite-sequence (list 1 2 3)))
         (check-true (list? ((iso g 1 VARIADIC-INPUT) 1 (list 1) (list 1))))
         (check-true (vector? ((iso g 1 VARIADIC-INPUT) 1 #(1) #(1))))
         (check-true (known-finite-sequence?
                      ((iso g 1 VARIADIC-INPUT) 1
                                                (known-finite-sequence (list 1 2 3))
                                                (known-finite-sequence (list 1 2 3))))
                     "custom type")
         (check-true (known-finite-sequence?
                      ((iso g 1 VARIADIC-INPUT) 1
                                                (opaque-sequence (list 1 2 3))
                                                (opaque-sequence (list 1 2 3))))
                     "output type is unchanged if input is opaque"))
       (test-case
           "opaque result"
         (define (g elem . seqs)
           (opaque-sequence (list 1 2 3)))
         (check-false (list? ((iso g 1 VARIADIC-INPUT) 1 (list 1))))
         (check-false (vector? ((iso g 1 VARIADIC-INPUT) 1 #(1))))
         (check-false (known-finite-sequence?
                       ((iso g 1 VARIADIC-INPUT) 1
                                                 (known-finite-sequence (list 1 2 3))
                                                 (known-finite-sequence (list 1 2 3))))
                      "custom type")))
      (test-suite
       "list input"
       (test-case
           "known finite result"
         (define (g elem seqs)
           (known-finite-sequence (list 1 2 3)))
         (check-true (list? ((iso g 1 LIST-INPUT) 1 (list (list 1)))))
         (check-true (vector? ((iso g 1 LIST-INPUT) 1 (list #(1) #(1)))))
         (check-true (known-finite-sequence?
                      ((iso g 1 LIST-INPUT) 1 (list (known-finite-sequence (list 1 2 3))
                                                    (known-finite-sequence (list 1 2 3)))))
                     "custom type")
         (check-true (known-finite-sequence?
                      ((iso g 1 LIST-INPUT) 1 (list (opaque-sequence (list 1 2 3))
                                                    (opaque-sequence (list 1 2 3)))))
                     "output type is unchanged if input is opaque"))
       (test-case
           "opaque result"
         (define (g elem seqs)
           (opaque-sequence (list 1 2 3)))
         (check-false (list? ((iso g 1 LIST-INPUT) 1 (list (list 1)))))
         (check-false (vector? ((iso g 1 LIST-INPUT) 1 (list #(1)))))
         (check-false (known-finite-sequence?
                       ((iso g 1 LIST-INPUT) 1
                                             (list (known-finite-sequence (list 1 2 3))
                                                   (known-finite-sequence (list 1 2 3)))))
                      "custom type")))
      (test-suite
       "two value result"
       (test-case
           "known finite result"
         (define (g elem seq)
           (values (known-finite-sequence (list 1 2 3))
                   (known-finite-sequence (list 1 2 3))))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 (list 1))])
           (check-true (list? a))
           (check-true (list? b)))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 #(1))])
           (check-true (vector? a))
           (check-true (vector? b)))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 (known-finite-sequence (list 1 2 3)))])
           (check-true (known-finite-sequence? a) "custom type")
           (check-true (known-finite-sequence? b) "custom type"))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 (opaque-sequence (list 1 2 3)))])
           (check-true (known-finite-sequence? a) "output type is unchanged if input is opaque")
           (check-true (known-finite-sequence? b) "output type is unchanged if input is opaque")))
       (test-case
           "opaque result"
         (define (g elem seq)
           (values (opaque-sequence (list 1 2 3))
                   (opaque-sequence (list 1 2 3))))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 (list 1 2 3))])
           (check-false (list? a))
           (check-false (list? b)))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 #(1))])
           (check-false (vector? a))
           (check-false (vector? b)))
         (let-values ([(a b) ((iso g 1 TWO-VALUE-RESULT) 1 (known-finite-sequence (list 1 2 3)))])
           (check-false (known-finite-sequence? a))
           (check-false (known-finite-sequence? b)))))
      (test-suite
       "sequence result"
       (test-case
           "known finite result"
         (define (g elem seq)
           (list (known-finite-sequence (list 1 2 3))
                 (known-finite-sequence (list 1 2 3))))
         (check-true (andmap list? ((iso g 1 SEQUENCE-RESULT) 1 (list 1))))
         (check-true (andmap vector? ((iso g 1 SEQUENCE-RESULT) 1 #(1))))
         (check-true (andmap known-finite-sequence?
                             ((iso g 1 SEQUENCE-RESULT) 1 (known-finite-sequence (list 1 2 3))))
                     "custom type")
         (check-true (andmap known-finite-sequence?
                             ((iso g 1 SEQUENCE-RESULT) 1 (opaque-sequence (list 1 2 3))))
                     "output type is unchanged if input is opaque"))
       (test-case
           "opaque result"
         (define (g elem seq)
           (list (opaque-sequence (list 1 2 3))
                 (opaque-sequence (list 1 2 3))))
         (check-false (andmap list? ((iso g 1 SEQUENCE-RESULT) 1 (list 1))))
         (check-false (andmap vector? ((iso g 1 SEQUENCE-RESULT) 1 #(1))))
         (check-false (andmap known-finite-sequence?
                       ((iso g 1 SEQUENCE-RESULT) 1 (known-finite-sequence (list 1 2 3))))
                      "custom type"))))

     ;; if the sequence is a string then elem is converted to a char
     ;; and is otherwise left alone
     (test-suite
      "string-helper"
      (test-case
          "no position indicated"
        (define (g elem seq)
          (char? elem))
        (check-true ((string-helper g) "a" "abc"))
        (check-false ((string-helper g) "a" (list 1))))
      (test-case
          "position indicated"
        (define (g elem seq)
          (char? elem))
        (check-true ((string-helper g 1 0) "a" "abc"))
        (check-false ((string-helper g 1 0) "a" (list 1))))))))

;;; data/collection / built-in
(define map (iso p:map 1))

;;; seq
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

(define interleave (iso p:interleave 0))

(define index-of (string-helper p:index-of))

(define deduplicate (iso p:deduplicate 0))

(module+ test
  (just-do
   (run-tests tests)))
