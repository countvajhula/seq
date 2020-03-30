#lang racket/base

(module+ test
  (require rackunit)
  (require racket/stream)
  (require racket/set)
  (require racket/function)
  (require (except-in data/collection
                      foldl
                      foldl/steps
                      append
                      index-of))
  (require relation))

;; Code here

(require "utils.rkt")

(provide (all-from-out "utils.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (->list (every 3 (list 1 2 3 4 5 6 7 8))) '(1 4 7))
  (check-equal? (->list (take-while even? (stream 2 4 1 3 5))) '(2 4))
  (check-equal? (->list (take-while even? (stream 1 3 5))) '())
  (check-equal? (->list (take-while odd? (stream 1 3 5))) '(1 3 5))
  (check-equal? (->list (take-until odd? (stream 2 4 1 3 5))) '(2 4))
  (check-equal? (->list (drop-while even? (stream 2 4 1 3 5))) '(1 3 5))
  (check-equal? (->list (drop-while even? (stream 1 3 5))) '(1 3 5))
  (check-equal? (->list (drop-while odd? (stream 1 3 5))) '())
  (check-equal? (->list (drop-until odd? (stream 2 4 1 3 5))) '(1 3 5))
  (check-equal? (->list (drop-until odd? (stream 2 4))) '())
  (check-equal? (->list (drop-until even? (stream 2 4))) '(2 4))
  (check-equal? (->set (deduplicate (list "hello" "Hello"))) (set "hello" "Hello"))
  (check-equal? (->set (deduplicate #:key string-upcase (list "hello" "Hello"))) (set "hello"))
  (check-equal? (->list (deduplicate (list 1 2 "hi" "hi" 2 3 "hello" 4 "hello" "bye"))) (list 1 2 "hi" 3 "hello" 4 "bye"))
  (check-equal? (starts-with? "hello" "hello there") #t)
  (check-equal? (starts-with? "h" "hello there") #t)
  (check-equal? (starts-with? "ello" "hello there") #f)
  (check-equal? (starts-with? "Hello" "hello there") #f)
  (check-equal? (starts-with? "Hello" "") #f)
  (check-equal? (starts-with? "" "Hello") #t)
  (check-equal? (starts-with? (list 1 2) (list 1 2 3 4 5)) #t)
  (check-equal? (starts-with? (list 2 1) (list 1 2 3 4 5)) #f)
  (check-equal? (starts-with? #:key (.. string-upcase ->string) "Hello" "hello there") #t)
  (check-equal? (ends-with? "there" "hello there") #t)
  (check-equal? (ends-with? "ere" "hello there") #t)
  (check-equal? (ends-with? "ther" "hello there") #f)
  (check-equal? (ends-with? "THERE" "hello there") #f)
  (check-equal? (ends-with? "Hello" "") #f)
  (check-equal? (ends-with? "" "Hello") #t)
  (check-equal? (ends-with? (list 5) (list 1 2 3 4 5)) #t)
  (check-equal? (ends-with? #:key (.. string-upcase ->string) "THERE" "hello there") #t)
  (check-equal? (find "hello there" "ello") 1)
  (check-equal? (find "hello there" " ") 5)
  (check-equal? (find "hello there" "elo") #f)
  (check-equal? (find "hello there" "Ello") #f)
  (check-equal? (find "" "Hello") #f)
  (check-equal? (find "Hello" "") 0)
  (check-equal? (find (list 1 2 3 4 5) (list 1 2)) 0)
  (check-equal? (find (list 1 2 3 4 5) (list 2 1)) #f)
  (check-equal? (find #:key (.. string-upcase ->string) "hello there" "Ello") 1)
  (check-equal? (->string (replace "hello there" "ello" "blah")) "hblah there")
  (check-equal? (->string (replace "hello there" " " ", ")) "hello, there")
  (check-equal? (->string (replace "hello there" "elo" "boop")) "hello there")
  (check-equal? (->string (replace "hello there" "Ello" "blah")) "hello there")
  (check-equal? (->string (replace "" "Hello" "Hi")) "")
  ;; (check-equal? (->string (replace "Hello" "" "Hi")) "HiHello") ; contractually exclude this
  (check-equal? (->list (replace (list 1 2 3 4 5) (list 1 2) (list 9 10))) (list 9 10 3 4 5))
  (check-equal? (replace (list 1 2 3 4 5) (list 2 1) (list 9 10)) (list 1 2 3 4 5))
  (check-equal? (->string (replace #:key (.. string-upcase ->string) "hello there" "Ello" "blah")) "hblah there")
  (check-equal? (contains? "hello there" "ello") #t)
  (check-equal? (contains? "hello there" " ") #t)
  (check-equal? (contains? "hello there" "elo") #f)
  (check-equal? (contains? "hello there" "Ello") #f)
  (check-equal? (contains? "" "Hello") #f)
  (check-equal? (contains? "Hello" "") #t)
  (check-equal? (contains? (list 1 2 3 4 5) (list 1 2)) #t)
  (check-equal? (contains? (list 1 2 3 4 5) (list 2 1)) #f)
  (check-equal? (contains? #:key (.. string-upcase ->string) "hello there" "Ello") #t)
  (check-equal? (trim-if (curry = 5) (list 1 2 3 4 5)) (list 1 2 3 4))
  (check-equal? (trim-if (curry = 5) (list 5 5 1 2 3 4 5 5 5)) (list 1 2 3 4))
  (check-equal? (->string (trim-if char-whitespace? "   \thello\n  ")) "hello")
  (check-equal? (->string (trim-if char-whitespace? "   \thello\n  " #:how-many #f)) "hello")
  (check-equal? (->string (trim-if char-whitespace? "  \thello\n  " #:how-many 1)) " \thello\n ")
  (check-equal? (->string (trim-if char-whitespace? "   \thello\n   " #:how-many 2)) " \thello\n ")
  (check-equal? (let-values ([(a b)
                              (split-at 2 (list 1 3 5 2 4))])
                  (->list (map ->list (list a b))))
                (list '(1 3) '(5 2 4)))
  (check-equal? (let-values ([(a b)
                              (split-at 5 "hellothere")])
                  (->list (map ->string (list a b))))
                (list "hello" "there"))
  (check-equal? (let-values ([(a b)
                              (split-where odd? (list 1 3 5 2 4))])
                  (->list (map ->list (list a b))))
                (list '() '(1 3 5 2 4)))
  (check-equal? (let-values ([(a b)
                              (split-where even? (list 1 3 5 2 4))])
                  (->list (map ->list (list a b))))
                (list '(1 3 5) '(2 4)))
  (check-equal? (->list (add-between (stream 'a 'b 'c) 'and)) '(a and b and c))
  (check-equal? (weave (stream "hi" "there") "\n") "hi\nthere")
  (check-equal? (weave (stream (list 1 2) (list 3 4)) (list 9)) (list 1 2 9 3 4))
  (check-equal? ((weave (list add1 sub1) add1) 5) 6)
  (check-equal? (index-of (list) 2) #f)
  (check-equal? (index-of (list 2) 2) 0)
  (check-equal? (index-of (list 1 2) 2) 1)
  (check-equal? (index-of (list 1 2) 3) #f)
  (check-equal? (index-of (list 1 2 2 1) 2) 1)
  (check-equal? (index-of (stream 1 2 2 1) 2) 1)
  (check-equal? (index-of #:key even? (list 1 4 2 3 6) 2) 1)
  (check-equal? (index-of #:key string-upcase (list "apple" "banana" "cherry") "BANANA") 1)
  (check-equal? (->list (remove (list 2) 2)) '())
  (check-equal? (->list (remove (list 2 1) 2)) (list 1))
  (check-equal? (->list (remove (list 1 2) 2)) (list 1))
  (check-equal? (->list (remove (list 1 2 2 1) 2)) (list 1 1))
  (check-equal? (->list (remove (stream 1 2 2 1) 2)) (list 1 1))
  (check-equal? (->list (remove #:how-many 1 (list 1 2 2 1) 2)) (list 1 2 1))
  (check-equal? (->list (remove #:how-many 2 (list 1 2 2 1 2) 2)) (list 1 1 2))
  (check-equal? (->list (remove #:key even? (list 1 2 4 3 6) 2)) (list 1 3))
  (check-equal? (->list (remove #:key even? #:how-many 2 (list 1 2 4 3 6) 2)) (list 1 3 6))
  (check-equal? (remove (set "apple" "banana" "cherry") "banana") (set "apple" "cherry"))
  (check-equal? (remove (generic-set #:key string-upcase "apple" "banana" "cherry") "BANANA") (generic-set #:key string-upcase "apple" "cherry"))

  (check-equal? (->list (remove-when odd? (list 2))) (list 2))
  (check-equal? (->list (remove-when even? (list 2))) '())
  (check-equal? (->list (remove-when even? (list 2 1))) (list 1))
  (check-equal? (->list (remove-when even? (list 1 2))) (list 1))
  (check-equal? (->list (remove-when even? (list 1 2 2 1))) (list 1 1))
  (check-equal? (->list (remove-when even? (stream 1 2 2 1))) (list 1 1))
  (check-equal? (->list (remove-when even? #:how-many 1 (list 1 2 2 1))) (list 1 2 1))
  (check-equal? (->list (remove-when even? #:how-many 2 (list 1 2 2 1 2))) (list 1 1 2))
  (check-equal? (->list (remove-when even? (list 1 2 4 3 6))) (list 1 3))
  (check-equal? (->list (remove-when even? #:how-many 2 (list 1 2 4 3 6))) (list 1 3 6))
  (check-exn exn:fail:contract?
             (thunk (remove-when (curry = "banana") (set "apple" "banana" "cherry"))) (set "apple" "cherry"))
  (check-exn exn:fail:contract?
             (thunk (remove-when (curry = "BANANA") (generic-set #:key string-upcase "apple" "banana" "cherry")))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
