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
                    index-where)
         relation
         seq/base)

(require "private/util.rkt")

(module+ test

  (define tests
    (test-suite
     "base tests"

     (test-case
         "by"
       (check-equal? (->list (by 3 (list 1 2 3 4 5 6 7 8))) '(1 4 7))
       (check-equal? (->list (by 3 (list 1 2 3 4))) '(1 4))
       (check-equal? (->list (by 3 (list 1 2 3))) '(1))
       (check-equal? (->list (by 3 (list))) '()))
     (test-case
         "init"
       (check-exn exn:fail:contract? (thunk (init (list))))
       (check-exn exn:fail:contract? (thunk (init empty-stream)))
       (check-equal? (->list (init (list 1))) '())
       (check-equal? (->list (init (list 1 2 3))) '(1 2))
       (check-equal? (->string (init "apple")) "appl")
       (check-equal? (->list (take 5 (init (naturals)))) '(0 1 2 3 4) "init is lazy"))
     (test-case
         "exists"
       (check-false (exists positive? (list -1)))
       (check-true (exists positive? (list 1)))
       (check-true (exists positive? (list -1 1 -2)))
       (check-false (exists positive? (list -1 -1 -2)))
       (check-false (exists positive? (list)))
       (check-false (exists < (list 2 3 4) (list 1 2 3)))
       (check-true (exists < (list 2 2 2) (list 1 2 3))))
     (test-case
         "for-all"
       (check-false (for-all positive? (list -1)))
       (check-true (for-all positive? (list 1)))
       (check-false (for-all positive? (list -1 1 -2)))
       (check-true (for-all positive? (list 1 1 2)))
       (check-false (for-all positive? (list 1 -1 2)))
       (check-true (for-all positive? (list)))
       (check-false (for-all < (list 2 3 4) (list 1 2 3)))
       (check-false (for-all < (list 2 2 2) (list 1 2 3)))
       (check-true (for-all < (list 1 2 3) (list 2 3 4))))
     (test-case
         "find"
       (check-equal? (find positive? (list -1 -2 3 -2 4)) 3)
       (check-equal? (find < (list 2 2 3) (list 2 3 4)) '(2 3))
       (check-equal? (find < (stream 2 2 3) (stream 2 3 4)) '(2 3))
       (check-false (find negative? (list 1 2 3)))
       (check-false (find < (list 2 3 4) (list 1 2 3)))
       (check-equal? (find positive? (list 2)) 2)
       (check-false (find positive? (list)))
       (check-false (find < (list) (list)))
       (check-false (find < (list 2 3 4) (list 1 2)))
       (check-equal? (find < (list 2 3) (list 1 4 2)) '(3 4)))
     (test-case
         "choose"
       (check-equal? (->list (choose positive? (list -1 -2 1 2) (list -5 3 -2) (list 5 2 -1))) '(1 3 5))
       (check-equal? (->list (choose positive? (stream -1 -2 1 2) (stream -5 3 -2) (stream 5 2 -1))) '(1 3 5))
       (check-equal? (->list (choose positive? (list -1 -2) (list -5 3 -2) (list 5 2 -1))) '(#f 3 5))
       (check-equal? (->list (choose positive? (list) (list -5 3 -2) (list 5 2 -1))) '(#f 3 5))
       (check-equal? (->list (choose positive? (list) (list -5 -2) (list 5 2 -1))) '(#f #f 5))
       (check-equal? (->list (choose positive? (list) (list))) '(#f #f))
       (check-equal? (->list (choose positive? (list))) '(#f))
       (check-equal? (->list (choose positive?)) '()))
     (test-case
         "take-while"
       (check-equal? (->list (take-while even? (stream 2 4 1 3 5))) '(2 4))
       (check-equal? (->list (take-while even? (stream 1 3 5))) '())
       (check-equal? (->list (take-while odd? (stream 1 3 5))) '(1 3 5)))
     (test-case
         "take-until"
       (check-equal? (->list (take-until odd? (stream 2 4 1 3 5))) '(2 4)))
     (test-case
         "drop-while"
       (check-equal? (->list (drop-while even? (stream 2 4 1 3 5))) '(1 3 5))
       (check-equal? (->list (drop-while even? (stream 1 3 5))) '(1 3 5))
       (check-equal? (->list (drop-while odd? (stream 1 3 5))) '()))
     (test-case
         "drop-until"
       (check-equal? (->list (drop-until odd? (stream 2 4 1 3 5))) '(1 3 5))
       (check-equal? (->list (drop-until odd? (stream 2 4))) '())
       (check-equal? (->list (drop-until even? (stream 2 4))) '(2 4)))
     (test-case
         "deduplicate"
       (check-equal? (deduplicate (list "hello" "Hello")) (list "hello" "Hello"))
       (check-equal? (deduplicate #:key string-upcase (list "hello" "Hello")) (list "hello"))
       (check-equal? (deduplicate (list 1 2 "hi" "hi" 2 3 "hello" 4 "hello" "bye")) (list 1 2 "hi" 3 "hello" 4 "bye")))
     (test-case
         "interleave"
       (check-equal? (->list (interleave (list 1 2 3) (list 'a 'b 'c) (list 'A 'B 'C))) '(1 a A 2 b B 3 c C))
       (check-equal? (->list (interleave (list 1 2 3) (list 'a 'b 'c) (list 'A 'B))) '(1 a A 2 b B 3 c))
       (check-equal? (->list (interleave (list 1) (list 'a) (list 'A))) '(1 a A))
       (check-equal? (->list (interleave (list 1) (list) (list 'A))) '(1))
       (check-equal? (->list (interleave (list) (list 'a) (list 'A))) '())
       (check-equal? (->list (interleave (list) (list) (list))) '())
       (check-equal? (->list (interleave (list))) '()))
     (test-case
         "truncate"
       (check-equal? (->list (truncate '(a b c) '(1 2))) '(a b))
       (check-equal? (->list (truncate '(a b c) '(1 2 3))) '(a b c))
       (check-equal? (->list (truncate '(a b c) '(1 2 3 4))) '(a b c))
       (check-equal? (->list (truncate '(a b c) '(1))) '(a))
       (check-equal? (->list (truncate '(a b c) '())) '())
       (check-equal? (->list (truncate '(a) '())) '())
       (check-equal? (->list (truncate '(a) '(1 2 3 4))) '(a))
       (check-equal? (->list (truncate '(a) (naturals))) '(a))
       (check-equal? (->list (truncate '() (naturals))) '())
       (check-equal? (->list (truncate '() '())) '()))
     (test-case
         "rotate-left"
       (check-equal? (->list (rotate-left 1 '(1 2 3))) '(2 3 1))
       (check-equal? (->list (rotate-left 2 '(1 2 3))) '(3 1 2))
       (check-equal? (->list (rotate-left 3 '(1 2 3))) '(1 2 3))
       (check-equal? (->list (rotate-left 4 '(1 2 3))) '(2 3 1))
       (check-equal? (->list (rotate-left 1 '(1))) '(1))
       (check-equal? (->list (rotate-left 2 '(1))) '(1))
       (check-equal? (->list (rotate-left 1 '())) '())
       (check-equal? (->list (rotate-left 0 '(1 2 3))) '(1 2 3))
       (check-equal? (->list (rotate-left 0 '())) '())
       (check-equal? (->list (rotate-left 1 (stream 1 2 3))) '(2 3 1)))
     (test-case
         "rotate-right"
       (check-equal? (->list (rotate-right 1 '(1 2 3))) '(3 1 2))
       (check-equal? (->list (rotate-right 2 '(1 2 3))) '(2 3 1))
       (check-equal? (->list (rotate-right 3 '(1 2 3))) '(1 2 3))
       (check-equal? (->list (rotate-right 4 '(1 2 3))) '(3 1 2))
       (check-equal? (->list (rotate-right 1 '(1))) '(1))
       (check-equal? (->list (rotate-right 2 '(1))) '(1))
       (check-equal? (->list (rotate-right 1 '())) '())
       (check-equal? (->list (rotate-right 0 '(1 2 3))) '(1 2 3))
       (check-equal? (->list (rotate-right 0 '())) '())
       (check-equal? (->list (rotate-right 1 (stream 1 2 3))) '(3 1 2)))
     (test-case
         "rotations"
       (check-equal? (->list (map ->list (rotations '(1 2 3)))) '((1 2 3) (2 3 1) (3 1 2)))
       (check-equal? (->list (map ->list (rotations '(1)))) '((1)))
       (check-equal? (->list (map ->list (rotations '()))) '()))
     (test-case
         "prefix?"
       (check-true (prefix? (list 1 2) (list 1 2 3)))
       (check-true (prefix? (list 1 2) (stream 1 2 3)))
       (check-true (prefix? "ap" "apricot"))
       (check-true (prefix? "" "apricot"))
       (check-true (prefix? "a" "apricot"))
       (check-false (prefix? "b" "apricot")))
     (test-case
         "suffix?"
       (check-true (suffix? (list 2 3) (list 1 2 3)))
       (check-true (suffix? (list 2 3) (stream 1 2 3)))
       (check-true (suffix? "ot" "apricot"))
       (check-true (suffix? "" "apricot"))
       (check-true (suffix? "t" "apricot"))
       (check-false (suffix? "b" "apricot")))
     (test-case
         "infix?"
       (check-true (infix? (list 1 2) (list 1 2 3 4)))
       (check-true (infix? (list 3 4) (list 1 2 3 4)))
       (check-true (infix? (list 2 3) (list 1 2 3 4)))
       (check-true (infix? (list 2 3) (stream 1 2 3 4)))
       (check-true (infix? "ic" "apricot"))
       (check-true (infix? "" "apricot"))
       (check-true (infix? "i" "apricot"))
       (check-false (infix? "b" "apricot")))
     (test-case
         "suffix"
       (check-equal? (->list (suffix 2 (list 1 2 3))) '(2 3))
       (check-equal? (->list (suffix 3 (range 10))) '(7 8 9))
       (check-equal? (->string (suffix 3 "hello")) "llo")
       (check-exn exn:fail:contract? (thunk (suffix 3 (list 1)))))
     (test-case
         "suffixes"
       (check-equal? (->list (suffixes (list 1 2 3))) '((1 2 3) (2 3) (3)))
       (check-equal? (->list (by 2 (suffixes (list 1 2 3 4 5)))) '((1 2 3 4 5) (3 4 5) (5)))
       (check-equal? (->list (suffixes (list))) '())
       (check-equal? (->list (suffixes (list 1))) '((1)))
       (check-equal? (->list (by 3 (suffixes (list 1 2)))) '((1 2)))
       (check-equal? (->list (map ->string (suffixes "hello"))) '("hello" "ello" "llo" "lo" "o")))
     (test-case
         "prefixes"
       (check-equal? (->list (map ->list (prefixes (list 1 2 3)))) '((1) (1 2) (1 2 3)))
       (check-equal? (->list (map ->list (by 2 (prefixes (list 1 2 3 4 5))))) '((1) (1 2 3) (1 2 3 4 5)))
       (check-equal? (->list (prefixes (list))) '())
       (check-equal? (->list (map ->list (prefixes (list 1)))) '((1)))
       (check-equal? (->list (map ->list (by 3 (prefixes (list 1 2))))) '((1)))
       (check-equal? (->list (map ->string (prefixes "hello"))) '("h" "he" "hel" "hell" "hello")))
     (test-case
         "infixes"
       (check-equal? (->list (map ->list (infixes 3 (list 1 2 3 4 5)))) '((1 2 3) (2 3 4) (3 4 5)))
       (check-equal? (->list (map ->list (infixes 3 (list 1 2)))) '())
       (check-equal? (->list (map ->list (infixes 3 (list 1)))) '())
       (check-equal? (->list (map ->list (infixes 3 (list)))) '()))
     (test-case
         "starts-with?"
       (check-equal? (starts-with? "hello" "hello there") #t)
       (check-equal? (starts-with? "h" "hello there") #t)
       (check-equal? (starts-with? "ello" "hello there") #f)
       (check-equal? (starts-with? "Hello" "hello there") #f)
       (check-equal? (starts-with? "Hello" "") #f)
       (check-equal? (starts-with? "" "Hello") #t)
       (check-equal? (starts-with? (list 1 2) (list 1 2 3 4 5)) #t)
       (check-equal? (starts-with? (list 2 1) (list 1 2 3 4 5)) #f)
       (check-equal? (starts-with? #:key (.. string-upcase ->string) "Hello" "hello there") #t))
     (test-case
         "ends-with?"
       (check-equal? (ends-with? "there" "hello there") #t)
       (check-equal? (ends-with? "ere" "hello there") #t)
       (check-equal? (ends-with? "ther" "hello there") #f)
       (check-equal? (ends-with? "THERE" "hello there") #f)
       (check-equal? (ends-with? "Hello" "") #f)
       (check-equal? (ends-with? "" "Hello") #t)
       (check-equal? (ends-with? (list 5) (list 1 2 3 4 5)) #t)
       (check-equal? (ends-with? #:key (.. string-upcase ->string) "THERE" "hello there") #t))
     (test-case
         "find-infix"
       (check-equal? (find-infix "ello" "hello there") 1)
       (check-equal? (find-infix " " "hello there") 5)
       (check-equal? (find-infix "elo" "hello there") #f)
       (check-equal? (find-infix "Ello" "hello there") #f)
       (check-equal? (find-infix "Hello" "") #f)
       (check-equal? (find-infix "" "Hello") 0)
       (check-equal? (find-infix (list 1 2) (list 1 2 3 4 5)) 0)
       (check-equal? (find-infix (list 2 1) (list 1 2 3 4 5)) #f)
       (check-equal? (find-infix #:key (.. string-upcase ->string) "Ello" "hello there") 1))
     (test-case
         "replace-infix"
       (check-equal? (->string (replace-infix "ello" "blah" "hello there")) "hblah there")
       (check-equal? (->string (replace-infix " " ", " "hello there")) "hello, there")
       (check-equal? (->string (replace-infix "elo" "boop" "hello there")) "hello there")
       (check-equal? (->string (replace-infix "Ello" "blah" "hello there")) "hello there")
       (check-equal? (->string (replace-infix "Hello" "Hi" "")) "")
       ;; (check-equal? (->string (replace-infix "Hello" "" "Hi")) "HiHello") ; contractually exclude this
       (check-equal? (->list (replace-infix (list 1 2) (list 9 10) (list 1 2 3 4 5))) (list 9 10 3 4 5))
       (check-equal? (replace-infix (list 2 1) (list 9 10) (list 1 2 3 4 5)) (list 1 2 3 4 5))
       (check-equal? (->string (replace-infix #:key (.. string-upcase ->string) "Ello" "blah" "hello there")) "hblah there"))
     (test-case
         "contains?"
       (check-equal? (contains? "ello" "hello there") #t)
       (check-equal? (contains? " " "hello there") #t)
       (check-equal? (contains? "elo" "hello there") #f)
       (check-equal? (contains? "Ello" "hello there") #f)
       (check-equal? (contains? "Hello" "") #f)
       (check-equal? (contains? "" "Hello") #t)
       (check-equal? (contains? (list 1 2) (list 1 2 3 4 5)) #t)
       (check-equal? (contains? (list 2 1) (list 1 2 3 4 5)) #f)
       (check-equal? (contains? #:key (.. string-upcase ->string) "Ello" "hello there") #t))
     (test-case
         "trim-if"
       (check-equal? (trim-if negative? (list)) (list))
       (check-equal? (trim-if negative? (list 1 2 3)) (list 1 2 3))
       (check-equal? (trim-if negative? (list -1 -2 1 2 3 -3)) (list 1 2 3))
       (check-equal? (trim-if negative? #:side 'left (list -1 -2 1 2 3 -3)) (list 1 2 3 -3))
       (check-equal? (trim-if negative? #:side 'right (list -1 -2 1 2 3 -3)) (list -1 -2 1 2 3))
       (check-equal? (trim-if (curry = 5) (list 1 2 3 4 5)) (list 1 2 3 4))
       (check-equal? (trim-if (curry = 5) (list 5 5 1 2 3 4 5 5 5)) (list 1 2 3 4))
       (check-equal? (->string (trim-if char-whitespace? "   \thello\n  ")) "hello")
       (check-equal? (->string (trim-if char-whitespace? "   \thello\n  " #:how-many #f)) "hello")
       (check-equal? (->string (trim-if char-whitespace? "  \thello\n  " #:how-many 1)) " \thello\n ")
       (check-equal? (->string (trim-if char-whitespace? "   \thello\n   " #:how-many 2)) " \thello\n "))
     (test-case
         "trim"
       (check-equal? (trim 0 (list)) (list))
       (check-equal? (trim 0 (list 1 2 3)) (list 1 2 3))
       (check-equal? (trim 0 (list 0 1 2 3 0 0)) (list 1 2 3))
       (check-equal? (trim 0 #:side 'left (list 0 1 2 3 0 0)) (list 1 2 3 0 0))
       (check-equal? (trim 0 #:side 'right (list 0 1 2 3 0 0)) (list 0 1 2 3))
       (check-equal? (->string (trim #\space "   \thello\n  ")) "\thello\n")
       (check-equal? (->string (trim #\h "hellohhh")) "ello"))
     (test-case
         "trim-by"
       (check-equal? (->list (trim-by 1 1 '(1 2 3))) '(2))
       (check-equal? (->list (trim-by 1 2 '(1 2 3 4 5))) '(2 3))
       (check-equal? (->list (trim-by 2 1 '(1 2 3 4 5))) '(3 4))
       (check-equal? (->list (trim-by 1 1 '(1 2))) '())
       (check-equal? (->list (trim-by 0 0 '())) '())
       (check-exn exn:fail:contract?
                  (thunk (trim-by 2 2 '(1 2 3)))))
     (test-case
         "cut-when"
       (check-equal? (->list (map ->string (cut-when (curry = #\space) "hello there"))) (list "hello" "there"))
       (check-equal? (->list (map ->string (cut-when (curry = #\space) "hello there old friend"))) (list "hello" "there" "old" "friend"))
       (check-equal? (->list (map ->string (cut-when (curry = #\space) " "))) (list ""))
       (check-equal? (->list (map ->string (cut-when #:trim? #f (curry = #\space) " "))) (list "" ""))
       (check-equal? (->list (map ->string (cut-when (curry = #\space) ""))) (list ""))
       (check-equal? (->list (map ->list (cut-when (curry = 1) (list 2 1 2)))) '((2) (2)))
       (check-equal? (->list (map ->list (cut-when (curry = 1) (list 2)))) '((2)))
       (check-equal? (->list (cut-when (curry = 1) (list))) (list ID)))
     (test-case
         "cut"
       (check-equal? (->list (map ->list (cut 5 (list 1 2 5 2 3 5 6 5 7 8)))) '((1 2) (2 3) (6) (7 8)))
       (check-equal? (->list (map ->list (cut 5 (list)))) '(()))
       (check-equal? (->list (map ->list (cut 5 (list 1)))) '((1)))
       (check-equal? (->list (map ->list (cut 5 (list 5)))) '(()))
       (check-equal? (->list (map ->list (cut #:trim? #f 5 (list 5)))) '(() ()))
       (check-equal? (->list (map ->list (cut 5 (list 1 2 3)))) '((1 2 3)))
       (check-equal? (->list (map ->list (cut #:trim? #t 5 (list 5 5 5 1 2 5 2 3 5 6 5 7 8 5 5 5)))) '((1 2) (2 3) (6) (7 8)))
       (check-equal? (->list (map ->list (cut #:trim? #f 5 (list 5 5 5 1 2 5 2 3 5 6 5 7 8 5 5 5)))) '(() () () (1 2) (2 3) (6) (7 8) () () ()))
       (check-equal? (->list (map ->list (cut #:trim? #t 5 (list 5 5 5 1 2 5 5 2 3 5 6 5 7 8 5 5 5)))) '((1 2) () (2 3) (6) (7 8)))
       (check-equal? (->list (map ->list (cut 5 (list 1 2 5 2 3 5 6 5)))) '((1 2) (2 3) (6)))
       (check-equal? (->list (map ->list (cut #:trim? #f 5 (list 1 2 5 2 3 5 6 5)))) '((1 2) (2 3) (6) ()))
       (check-equal? (->list (map ->string (cut #\newline "hello\n there"))) (list "hello" " there")))
     (test-case
         "cut-at"
       (check-equal? (let-values ([(a b)
                                   (cut-at 2 (list 1 3 5 2 4))])
                       (->list (map ->list (list a b))))
                     (list '(1 3) '(5 2 4)))
       (check-equal? (let-values ([(a b)
                                   (cut-at 5 "hellothere")])
                       (->list (map ->string (list a b))))
                     (list "hello" "there")))
     (test-case
         "cut-where"
       (check-equal? (let-values ([(a b)
                                   (cut-where odd? (list 1 3 5 2 4))])
                       (->list (map ->list (list a b))))
                     (list '() '(1 3 5 2 4)))
       (check-equal? (let-values ([(a b)
                                   (cut-where even? (list 1 3 5 2 4))])
                       (->list (map ->list (list a b))))
                     (list '(1 3 5) '(2 4))))
     (test-case
         "cut-by"
       (check-equal? (->list (map ->list (cut-by 2 (list 1 2 3 4 5 6)))) '((1 2) (3 4) (5 6)))
       (check-equal? (->list (map ->list (cut-by 2 (list 1 2 3 4 5)))) '((1 2) (3 4)))
       (check-equal? (->list (map ->list (cut-by 3 (list 1 2 3 4 5)))) '((1 2 3)))
       (check-equal? (->list (map ->list (cut-by 3 (list 1 2 3)))) '((1 2 3)))
       (check-equal? (->list (map ->list (cut-by 3 (list 1 2)))) '())
       (check-equal? (->list (map ->list (cut-by 3 (list 1)))) '())
       (check-equal? (->list (map ->list (cut-by 3 (list)))) '())
       (check-equal? (->list (map ->list (cut-by 5 (range 12)))) '((0 1 2 3 4) (5 6 7 8 9)) "cut a lazy sequence"))
     (test-case
         "cut-with"
       (let-values ([(yes no) (cut-with positive? (list))])
         (check-equal? (->list yes) '())
         (check-equal? (->list no) '()))
       (let-values ([(yes no) (cut-with positive? (list 1))])
         (check-equal? (->list yes) '(1))
         (check-equal? (->list no) '()))
       (let-values ([(yes no) (cut-with positive? (list -1))])
         (check-equal? (->list yes) '())
         (check-equal? (->list no) '(-1)))
       (let-values ([(yes no) (cut-with positive? (list 1 -2))])
         (check-equal? (->list yes) '(1))
         (check-equal? (->list no) '(-2)))
       (let-values ([(yes no) (cut-with positive? (list -1 2 -3 -4 5 6 -7))])
         (check-equal? (->list yes) '(2 5 6))
         (check-equal? (->list no) '(-1 -3 -4 -7))))
     (test-case
         "intersperse"
       (check-equal? (->list (intersperse 'and (stream 'a 'b 'c))) '(a and b and c))
       (check-equal? (->list (intersperse 'and '(x))) '(x))
       (check-equal? (->list (add-between "," '("a" "b" "c" "d"))) '("a" "," "b" "," "c" "," "d")))
     (test-case
         "wrap-each"
       (check-equal? (->list (wrap-each '< '> (stream 'a 'b 'c))) '(< a > < b > < c >)))
     (test-case
         "join-with"
       (check-equal? (join-with "\n" (stream "hi" "there")) "hi\nthere")
       (check-equal? (join-with (list 9) (stream (list 1 2) (list 3 4))) (list 1 2 9 3 4))
       (check-equal? ((join-with add1 (list add1 sub1)) 5) 6))
     (test-case
         "weave"
       (check-equal? ((weave ->string ->number (list add1 (power add1 2) (power add1 3))) "7") "13"))
     (test-case
         "multiples"
       (check-equal? (->list (take 4 (multiples 3))) '(0 3 6 9))
       (check-equal? (->list (take 4 (multiples 3 1))) '(3 6 9 12))
       (check-equal? (->list (take 4 (multiples 1))) '(0 1 2 3))
       (check-equal? (->list (take 4 (multiples 0))) '(0 0 0 0)))
     (test-case
         "powers"
       (check-equal? (->list (onto (take 4 (powers add1)) 3)) '(3 4 5 6))
       (check-equal? (->list (take 4 (powers 3))) '(0 3 6 9))
       (check-equal? (->list (take 4 (powers 3 *))) '(1 3 9 27))
       (check-equal? (->list (take 4 (powers "abc"))) (list "" "abc" "abcabc" "abcabcabc"))
       (check-equal? (->list (take 4 (powers '(1 2 3)))) (list '() '(1 2 3) '(1 2 3 1 2 3) '(1 2 3 1 2 3 1 2 3))))
     (test-case
         "iterate"
       (check-equal? (->list (take 4 (iterate add1 3))) '(3 4 5 6))
       (check-equal? (->list (take 4 (iterate sqr 2))) '(2 4 16 256)))
     (test-case
         "zip"
       (check-equal? (->list (zip (list 'a 'b 'c) (list 1 2 3))) (list (list 'a 1) (list 'b 2) (list 'c 3))))
     (test-case
         "zip-with"
       (check-equal? (->list (zip-with list (list 1 2 3) (list 1 2 3))) '((1 1) (2 2) (3 3)))
       (check-equal? (->list (zip-with list (list 1 2 3) (list 1 2 3) (list 1 2 3))) '((1 1 1) (2 2 2) (3 3 3)))
       (check-equal? (->list (zip-with list (list 1 2 3))) '((1) (2) (3)))
       (check-equal? (->list (zip-with list (list) (list))) '())
       (check-equal? (->list (zip-with string "hello" "there")) (list "ht" "eh" "le" "lr" "oe"))
       (check-equal? (->list (zip-with list (list 1 2 3) (list 1 2 3 4))) '((1 1) (2 2) (3 3)) "sequences of unequal length")
       (check-equal? (->list (zip-with list (list 1 2 3 4) (list 1 2 3))) '((1 1) (2 2) (3 3)) "sequences of unequal length")
       (check-equal? (->list (zip-with list (naturals 1) (list 'a 'b 'c))) '((1 a) (2 b) (3 c))))
     (test-case
         "zip-unzip sanity"
       (let ([seqs (list
                    (list (list 1 2 3) (list 1 2 3))
                    (list (list 1 2 3) (list 1 2 3) (list 1 2 3))
                    (list (list 1 2 3)))])
         (for-each (λ (seq)
                     (check-equal? (->list (unzip-with list (apply zip-with list seq)))
                                   seq))
                   seqs))
       (check-equal? (->list (unzip-with string (zip-with (.. string->immutable-string string) "hello" "there")))
                     (list "hello" "there"))
       (check-equal? (->list (unzip-with list (zip-with list (list 1 2 3) (list 1 2 3 4))))
                     (list (list 1 2 3) (list 1 2 3)))
       (check-equal? (->list (unzip-with list (zip-with list (list 1 2 3 4) (list 1 2 3))))
                     (list (list 1 2 3) (list 1 2 3)))
       (check-equal? (->list (unzip-with list (zip-with list (naturals 1) (list 'a 'b 'c))))
                     (list (list 1 2 3) (list 'a 'b 'c))))
     (test-case
         "index-of"
       (check-equal? (index-of 2 (list)) #f)
       (check-equal? (index-of 2 (list 2)) 0)
       (check-equal? (index-of 2 (list 1 2)) 1)
       (check-equal? (index-of 3 (list 1 2)) #f)
       (check-equal? (index-of 2 (list 1 2 2 1)) 1)
       (check-equal? (index-of 2 (stream 1 2 2 1)) 1)
       (check-equal? (index-of #:key even? 2 (list 1 4 2 3 6)) 1)
       (check-equal? (index-of #:key string-upcase "BANANA" (list "apple" "banana" "cherry")) 1))
     (test-case
         "index-where"
       (check-equal? (index-where positive? (list -1 0 1 2)) 2)
       (check-equal? (index-where positive? (list 1 2)) 0)
       (check-equal? (index-where positive? (list -1 2)) 1)
       (check-equal? (index-where positive? (list -1)) #f)
       (check-equal? (index-where positive? (list 1)) 0)
       (check-equal? (index-where positive? (list -1 0 -1 -2)) #f)
       (check-equal? (index-where positive? (list)) #f)
       (check-equal? (index-where > (list 1 2 3 4) (list 1 3 2 3)) 2)
       (check-equal? (index-where > (list 1 2) (list 1 3 2 3)) #f)
       (check-equal? (index-where > (list 1 2 3 4) (list 1 2)) #f)
       (check-equal? (index-where > (list 1 4) (list 1 3 2 3)) 1)
       (check-equal? (index-where > (list 1 3 2 4) (list 1 2)) 1)
       (check-equal? (index-where > null null) #f))
     (test-case
         "remove"
       (check-equal? (->list (remove 2 (list 2))) '())
       (check-equal? (->list (remove 2 (list 2 1))) (list 1))
       (check-equal? (->list (remove 2 (list 1 2))) (list 1))
       (check-equal? (->list (remove 2 (list 1 2 2 1))) (list 1 1))
       (check-equal? (->list (remove 2 (stream 1 2 2 1))) (list 1 1))
       (check-equal? (->list (remove #:how-many 1 2 (list 1 2 2 1))) (list 1 2 1))
       (check-equal? (->list (remove #:how-many 2 2 (list 1 2 2 1 2))) (list 1 1 2))
       (check-equal? (->list (remove #:key even? 2 (list 1 2 4 3 6))) (list 1 3))
       (check-equal? (->list (remove #:key even? #:how-many 2 2 (list 1 2 4 3 6))) (list 1 3 6))
       (check-equal? (remove "banana" (set "apple" "banana" "cherry")) (set "apple" "cherry"))
       (check-equal? (remove "BANANA" (generic-set #:key string-upcase "apple" "banana" "cherry")) (generic-set #:key string-upcase "apple" "cherry"))
       (check-equal? (->string (remove #\a "aaahai athaerea")) "hi there"))
     (test-case
         "remove-at"
       (check-equal? (->list (remove-at 0 (list 1 2 3))) (list 2 3))
       (check-equal? (->list (remove-at 1 (list 1 2 3))) (list 1 3))
       (check-equal? (->list (remove-at 2 (list 1 2 3))) (list 1 2))
       (check-exn exn:fail:contract? (thunk (remove-at 3 (list 1 2 3)))))
     (test-case
         "drop-when"
       (check-equal? (->list (drop-when odd? (list 2))) (list 2))
       (check-equal? (->list (drop-when even? (list 2))) '())
       (check-equal? (->list (drop-when even? (list 2 1))) (list 1))
       (check-equal? (->list (drop-when even? (list 1 2))) (list 1))
       (check-equal? (->list (drop-when even? (list 1 2 2 1))) (list 1 1))
       (check-equal? (->list (drop-when even? (stream 1 2 2 1))) (list 1 1))
       (check-equal? (->list (drop-when even? #:how-many 1 (list 1 2 2 1))) (list 1 2 1))
       (check-equal? (->list (drop-when even? #:how-many 2 (list 1 2 2 1 2))) (list 1 1 2))
       (check-equal? (->list (drop-when even? (list 1 2 4 3 6))) (list 1 3))
       (check-equal? (->list (drop-when even? #:how-many 2 (list 1 2 4 3 6))) (list 1 3 6))
       (check-exn exn:fail:contract?
                  (thunk (drop-when (curry = "banana") (set "apple" "banana" "cherry"))) (set "apple" "cherry"))
       (check-exn exn:fail:contract?
                  (thunk (drop-when (curry = "BANANA") (generic-set #:key string-upcase "apple" "banana" "cherry"))))))))

(module+ test
  (just-do
   (run-tests tests)))