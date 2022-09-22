#lang racket/base

(require racket/set
         arguments
         (only-in racket/function curry)
         (only-in racket/stream stream?)
         (for-syntax racket/base
                     arguments)
         (only-in data/collection
                  sequence->list
                  apply
                  andmap
                  [map d:map]
                  [take d:take]
                  known-finite?
                  first
                  nth
                  [set-nth d:set-nth]
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

(provide map
         filter
         reverse
         rest
         take
         drop
         set-nth
         by
         take-when
         prefix
         suffix-at
         infix
         infix-at
         init
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
         index-of
         drop-when
         intersperse
         add-between
         wrap-each
         interleave
         deduplicate
         (rename-out [p:range range]
                     [p:nth nth]
                     [p:exists exists]
                     [p:for-all for-all]
                     [p:find find]
                     [p:index-where index-where]
                     [p:zip-with zip-with]
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

(define (string-helper source arg)
  (if (string? source)
      (->char arg)
      arg))

(define (return source result)
  (cond [(and source (list? source) (known-finite? result)) (->list result)]
        [(and source (string? source) (known-finite? result)) (->string result)]
        [(and source (vector? source) (known-finite? result)) (->vector result)]
        [(and source (bytes? source) (known-finite? result)) (->bytes result)]
        [(and source (set? source) (known-finite? result)) (->set result)]
        [(and source (hash? source) (known-finite? result)) (->hash result)]
        [(and source (stream? source) (known-finite? result)) (->stream result)]
        [(and source (collection? source) (known-finite? result))
         (let ([null-cons (appendable-identity source)])
           (extend null-cons result))]
        [else result]))

(module+ test

  ;; the main test module for this is tests/iso.rkt
  ;; but we use a test submodule here to avoid providing
  ;; the `return` and `string-helper` functions outside this module
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
      "isomorphic result"
      (check-true (opaque-sequence? (return (list 1 2 3) (opaque-sequence null))))
      (check-true (opaque-sequence? (return "hello" (opaque-sequence null))))
      (check-true (opaque-sequence? (return #(1 2 3) (opaque-sequence null))))
      (check-true (opaque-sequence? (return #"hello" (opaque-sequence null))))
      (check-true (opaque-sequence? (return (set 1 2 3) (opaque-sequence null))))
      (check-true (opaque-sequence? (return (hash 'a 1 'b 2 'c 3) (opaque-sequence null))))
      (check-true (opaque-sequence? (return (stream 1 2 3) (opaque-sequence null))))
      (check-true (list? (return (list 1 2 3) (known-finite-sequence null))))
      (check-true (string? (return "hello" (known-finite-sequence null))))
      (check-true (vector? (return #(1 2 3) (known-finite-sequence null))))
      (check-true (bytes? (return #"hello" (known-finite-sequence null))))
      (check-true (set? (return (set 1 2 3) (known-finite-sequence null))))
      ;; (check-true (hash? (return (hash 'a 1 'b 2 'c 3) (known-finite-sequence null))))
      (check-true (stream? (return (stream 1 2 3) (known-finite-sequence null)))))

     ;; if the sequence is a string then elem is converted to a char
     ;; and is otherwise left alone
     (test-suite
      "string-helper"
      (check-false (char? (string-helper (list 1 2 3) "a")))
      (check-false (char? (string-helper #(1 2 3) "a")))
      (check-true (char? (string-helper "hello" "a")))))))

(define-syntax-parser define-isomorphic
  [(_ fname f 1)
   ;; function taking exactly one argument
   ;; which is the sequence itself
   #'(define (fname seq)
       (let ([result (f seq)])
         (return seq result)))]
  [(_ fname f 2 1)
   ;; function taking two arguments, with the sequence
   ;; at position 1 (0-indexed)
   #'(define (fname arg seq)
       (let ([result (f arg seq)])
         (return seq result)))]
  [(_ fname f 2 1 (~datum STRING-HELPER))
   ;; function taking two arguments, with the sequence
   ;; at position 1 (0-indexed)
   #'(define (fname arg seq)
       (let ([arg (string-helper seq arg)])
         (let ([result (f arg seq)])
           (return seq result))))]
  [(_ fname f 2 0)
   ;; function taking two arguments, with the sequence
   ;; at position 0 (0-indexed)
   #'(define (fname seq arg)
       (let ([result (f seq arg)])
         (return seq result)))]
  [(_ fname f 3 2)
   ;; function taking three arguments, with the sequence
   ;; at position 2 (0-indexed)
   #'(define (fname arg1 arg2 seq)
       (let ([result (f arg1 arg2 seq)])
         (return seq result)))]
  [(_ fname f 3 2 (~datum STRING-HELPER))
   ;; function taking three arguments, with the sequence
   ;; at position 2 (0-indexed)
   #'(define (fname arg1 arg2 seq)
       (let* ([arg1 (string-helper seq arg1)]
              [arg2 (string-helper seq arg2)]
              [result (f arg1 arg2 seq)])
         (return seq result)))]
  [(_ fname f (~datum VARIADIC-INPUT))
   ;; function taking any number of sequence arguments
   #'(define (fname . seqs)
       (let ([result (apply f seqs)]
             [seq (first seqs)])
         (return seq result)))]
  [(_ fname f 1 (~datum VARIADIC-INPUT))
   ;; function taking an arbitrary argument, followed by
   ;; any number of sequence arguments
   #'(define (fname arg . seqs)
       (let ([result (apply f arg seqs)]
             [seq (first seqs)])
         (return seq result)))]
  [(_ fname f (~datum LIST-INPUT))
   ;; function taking a list of sequences as its sole argument
   #'(define (fname seqs)
       (let ([result (f seqs)]
             [seq (first seqs)])
         (return seq result)))]
  [(_ fname f 1 (~datum LIST-INPUT))
   ;; function taking an arbitrary argument, followed by
   ;; a list of sequences as its second argument
   #'(define (fname arg seqs)
       (let ([result (f arg seqs)]
             [seq (first seqs)])
         (return seq result)))]
  [(_ fname f 2 1 (~datum TWO-VALUE-RESULT))
   ;; function taking an arbitrary argument followed by
   ;; a sequence, returning two values, each a sequence
   ;; TODO: what about the wrapping sequence in a sequence of sequences?
   #'(define (fname arg seq)
       (let-values ([(a b) (f arg seq)])
         (values (return seq a)
                 (return seq b))))]
  [(_ fname f 1 (~datum SEQUENCE-RESULT))
   ;; function taking a single sequence argument
   ;; that returns a sequence of sequences
   #'(define (fname seq)
       (let ([result (f seq)])
         (d:map (curry return seq) result)))]
  [(_ fname f 2 1 (~datum SEQUENCE-RESULT))
   ;; function taking a single sequence argument
   ;; that returns a sequence of sequences
   #'(define (fname arg seq)
       (let ([result (f arg seq)])
         (d:map (curry return seq) result)))])

;;; built-in or data/collection sequences
(define-isomorphic map p:map 1 VARIADIC-INPUT)

(define-isomorphic filter p:filter 2 1)

(define-isomorphic reverse p:reverse 1)

(define-isomorphic rest p:rest 1)

(define-isomorphic take d:take 2 1)

(define-isomorphic drop p:drop 2 1)

(define (set-nth n v seq)
  (let* ([v (string-helper seq v)]
         [result (p:set-nth n v seq)])
    (return seq result)))

;;; seq
(define-isomorphic by p:by 2 1)

(define (index-of #:key [key #f]
                  elem
                  seq)
  (let* ([elem (string-helper seq elem)]
         [result (p:index-of #:key key elem seq)])
    result))

(define-isomorphic take-when p:take-when 2 1)

(define-isomorphic prefix p:prefix 2 1)

(define-isomorphic suffix-at p:suffix-at 2 1)

(define-isomorphic infix-at p:infix-at 3 2)

(define-isomorphic infix p:infix 3 2)

(define-isomorphic init p:init 1)

(define (zip . seqs)
  (let ([result (apply p:zip seqs)])
    (p:map (curry return (first seqs)) result)))

(define-isomorphic unzip-with p:unzip-with 1 LIST-INPUT)

(define-isomorphic unzip p:unzip LIST-INPUT)

(define-isomorphic choose p:choose 1 VARIADIC-INPUT)

(define-isomorphic suffix p:suffix 2 1)

(define-isomorphic take-while p:take-while 2 1)

(define-isomorphic drop-while p:drop-while 2 1)

(define-isomorphic take-until p:take-until 2 1)

(define-isomorphic drop-until p:drop-until 2 1)

(define (cut-when pred seq)
  (d:map (curry return seq)
         (p:cut-when pred seq)))

(define (cut #:key [key #f] elem seq)
  (d:map (curry return seq)
         (p:cut #:key key
                (string-helper seq elem)
                seq)))

(define-isomorphic cut-at p:cut-at 2 1 TWO-VALUE-RESULT)

(define-isomorphic cut-where p:cut-where 2 1 TWO-VALUE-RESULT)

(define-isomorphic cut-by p:cut-by 2 1 SEQUENCE-RESULT)

(define-isomorphic cut-with p:cut-with 2 1 TWO-VALUE-RESULT)

(define-isomorphic truncate p:truncate 2 0)

(define-isomorphic rotate-left p:rotate-left 2 1)

(define-isomorphic rotate-right p:rotate-right 2 1)

(define-isomorphic rotate p:rotate 1)

(define-isomorphic rotations p:rotations 1 SEQUENCE-RESULT)

(define-isomorphic prefixes p:prefixes 1 SEQUENCE-RESULT)

(define-isomorphic suffixes p:suffixes 1 SEQUENCE-RESULT)

(define-isomorphic infixes p:infixes 2 1 SEQUENCE-RESULT)

;; not sure about this one
(define (replace-infix #:key [key #f]
                       #:how-many [how-many #f]
                       orig-subseq
                       new-subseq
                       seq)
  (return seq
          (p:replace-infix #:key key
                           #:how-many how-many
                           orig-subseq
                           new-subseq
                           seq)))

(define-isomorphic trim-if p:trim-if 2 1)

(define-isomorphic trim p:trim 2 1 STRING-HELPER)

(define-isomorphic trim-by p:trim-by 3 2)

(define (remove #:key [key #f]
                #:how-many [how-many #f]
                elem
                seq)
  (return seq
          (p:remove #:key key
                    #:how-many how-many
                    (string-helper seq elem)
                    seq)))

(define-isomorphic remove-at p:remove-at 2 1)

(define (drop-when #:how-many [how-many #f]
                   pred
                   seq)
  (return seq
          (p:drop-when #:how-many how-many
                       pred
                       seq)))

(define-isomorphic intersperse p:intersperse 2 1 STRING-HELPER)

(define-isomorphic add-between p:add-between 2 1 STRING-HELPER)

(define-isomorphic wrap-each p:wrap-each 3 2 STRING-HELPER)

(define-isomorphic interleave p:interleave VARIADIC-INPUT)

(define (deduplicate seq #:key [key #f])
  (return seq
          (p:deduplicate #:key key
                         seq)))

(module+ test
  (just-do
   (run-tests tests)))
