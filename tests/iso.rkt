#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/stream
         racket/set
         racket/math
         (only-in racket/function
                  thunk)
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of
                    index-where
                    range)
         relation
         seq/iso)

(require "private/util.rkt")

(module+ test

  (define tests
    (test-suite
     "iso tests"
     (test-suite
      "iso macro tests")
     (test-suite
      "smoke integration tests"

      (test-case
          "by"
        (check-equal? (by 3 (list 1 2 3 4 5 6 7 8)) '(1 4 7)))
      (test-case
          "init"
        (check-equal? (init (list 1 2 3)) '(1 2)))
      (test-case
          "exists"
        (check-true (exists positive? (list -1 1 -2))))
      (test-case
          "for-all"
        (check-true (for-all positive? (list 1 1 2))))
      (test-case
          "find"
        (check-equal? (find positive? (list -1 -2 3 -2 4)) 3))
      (test-case
          "choose"
        (check-equal? (choose positive? (list -1 -2 1 2) (list -5 3 -2) (list 5 2 -1)) '(1 3 5)))
      (test-case
          "take-while"
        (check-equal? (take-while even? (list 2 4 1 3 5)) '(2 4)))
      (test-case
          "take-until"
        (check-equal? (take-until odd? (list 2 4 1 3 5)) '(2 4)))
      (test-case
          "drop-while"
        (check-equal? (drop-while even? (list 2 4 1 3 5)) '(1 3 5)))
      (test-case
          "drop-until"
        (check-equal? (drop-until odd? (list 2 4 1 3 5)) '(1 3 5)))
      (test-case
          "deduplicate"
        (check-equal? (deduplicate #:key string-upcase (list "hello" "Hello")) (list "hello")))
      (test-case
          "interleave"
        (check-equal? (interleave (list 1 2 3) (list 'a 'b 'c) (list 'A 'B 'C)) '(1 a A 2 b B 3 c C)))
      (test-case
          "truncate"
        (check-equal? (truncate '(a b c) '(1 2)) '(a b)))
      (test-case
          "rotate-left"
        (check-equal? (rotate-left 1 '(1 2 3)) '(2 3 1)))
      (test-case
          "rotate-right"
        (check-equal? (rotate-right 1 '(1 2 3)) '(3 1 2)))
      (test-case
          "rotations"
        (check-equal? (->list (rotations '(1 2 3))) '((1 2 3) (2 3 1) (3 1 2))))
      (test-case
          "prefix?"
        (check-true (prefix? "ap" "apricot")))
      (test-case
          "suffix?"
        (check-true (suffix? "ot" "apricot")))
      (test-case
          "infix?"
        (check-true (infix? "ic" "apricot")))
      (test-case
          "suffix"
        (check-equal? (suffix 3 "hello") "llo"))
      (test-case
          "suffixes"
        (check-equal? (->list (suffixes (list 1 2 3))) '((1 2 3) (2 3) (3))))
      (test-case
          "prefixes"
        (check-equal? (->list (prefixes (list 1 2 3))) '((1) (1 2) (1 2 3))))
      (test-case
          "infixes"
        (check-equal? (->list (infixes 3 (list 1 2 3 4 5))) '((1 2 3) (2 3 4) (3 4 5))))
      (test-case
          "starts-with?"
        (check-equal? (starts-with? "hello" "hello there") #t))
      (test-case
          "ends-with?"
        (check-equal? (ends-with? "there" "hello there") #t))
      (test-case
          "find-infix"
        (check-equal? (find-infix "ello" "hello there") 1))
      (test-case
          "replace-infix"
        (check-equal? (replace-infix "ello" "blah" "hello there") "hblah there"))
      (test-case
          "contains?"
        (check-equal? (contains? "ello" "hello there") #t))
      (test-case
          "trim-if"
        (check-equal? (trim-if negative? (list -1 -2 1 2 3 -3)) (list 1 2 3)))
      (test-case
          "trim"
        (check-equal? (trim 0 (list 0 1 2 3 0 0)) (list 1 2 3))
        (check-equal? (trim " " "   \thello\n  ") "\thello\n")
        (check-equal? (trim "h" "hellohhh") "ello"))
      (test-case
          "trim-by"
        (check-equal? (trim-by 1 1 '(1 2 3)) '(2)))
      (test-case
          "cut-when"
        (check-equal? (->list (cut-when (curry = #\space) "hello there old friend")) (list "hello" "there" "old" "friend")))
      (test-case
          "cut"
        (check-equal? (->list (cut 5 (list 1 2 5 2 3 5 6 5 7 8))) '((1 2) (2 3) (6) (7 8)))
        (check-equal? (->list (cut "\n" "hello\n there")) (list "hello" " there") "cut string handles string separator as char"))
      (test-case
          "cut-at"
        (check-equal? (let-values ([(a b)
                                    (cut-at 5 "hellothere")])
                        (list a b))
                      (list "hello" "there")))
      (test-case
          "cut-where"
        (check-equal? (let-values ([(a b)
                                    (cut-where even? (list 1 3 5 2 4))])
                        (list a b))
                      (list '(1 3 5) '(2 4))))
      (test-case
          "cut-by"
        (check-equal? (->list (cut-by 2 (list 1 2 3 4 5 6))) '((1 2) (3 4) (5 6))))
      (test-case
          "cut-with"
        (let-values ([(yes no) (cut-with positive? (list -1 2 -3 -4 5 6 -7))])
          (check-equal? yes '(2 5 6))
          (check-equal? no '(-1 -3 -4 -7))))
      (test-case
          "intersperse"
        (check-equal? (intersperse 'and (list 'a 'b 'c)) '(a and b and c)))
      (test-case
          "wrap-each"
        (check-equal? (wrap-each '< '> (list 'a 'b 'c)) '(< a > < b > < c >)))
      (test-case
          "join-with"
        (check-equal? (join-with "\n" (stream "hi" "there")) "hi\nthere"))
      (test-case
          "weave"
        (check-equal? ((weave ->string ->number (list add1 (power add1 2) (power add1 3))) "7") "13"))
      (test-case
          "multiples"
        (check-equal? (->list (take 4 (multiples 3))) '(0 3 6 9)))
      (test-case
          "powers"
        (check-equal? (->list (onto (take 4 (powers add1)) 3)) '(3 4 5 6)))
      (test-case
          "iterate"
        (check-equal? (->list (take 4 (iterate add1 3))) '(3 4 5 6)))
      (test-case
          "zip"
        (check-equal? (zip (list 'a 'b 'c) (list 1 2 3)) (list (list 'a 1) (list 'b 2) (list 'c 3))))
      (test-case
          "zip-with"
        (check-equal? (zip-with list (list 1 2 3) (list 1 2 3)) '((1 1) (2 2) (3 3))))
      (test-case
          "zip-unzip sanity"
        (let ([seqs (list
                     (list (list 1 2 3) (list 1 2 3))
                     (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
                     (list (list 1 2 3)))])
          (for-each (λ (seq)
                      (check-equal? (unzip-with list (apply zip-with list seq))
                                    seq))
                    seqs)))
      (test-case
          "index-of"
        (check-equal? (index-of 2 (list 1 2 2 1)) 1))
      (test-case
          "index-where"
        (check-equal? (index-where positive? (list -1 0 1 2)) 2))
      (test-case
          "remove"
        (check-equal? (remove 2 (list 1 2 2 1)) (list 1 1)))
      (test-case
          "remove-at"
        (check-equal? (remove-at 1 (list 1 2 3)) (list 1 3)))
      (test-case
          "drop-when"
        (check-equal? (drop-when even? (list 1 2 2 1)) (list 1 1)))))))

(module+ test
  (just-do
   (run-tests tests)))