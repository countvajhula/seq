#lang racket/base

(module+ test
  (require rackunit)
  (require racket/stream)
  (require racket/function)
  (require (except-in data/collection
                      foldl
                      foldl/steps
                      append))
  (require relation))

;; Code here

(require "utils.rkt")

(provide (all-from-out "utils.rkt"))

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (->list (every 3 (list 1 2 3 4 5 6 7 8))) '(1 4 7))
  (check-equal? (->list (takef (stream 2 4 1 3 5) even?)) '(2 4))
  (check-equal? (->list (dropf (stream 2 4 1 3 5) even?)) '(1 3 5))
  (check-equal? (starts-with? "hello there" "hello") #t)
  (check-equal? (starts-with? "hello there" "h") #t)
  (check-equal? (starts-with? "hello there" "ello") #f)
  (check-equal? (starts-with? "hello there" "Hello") #f)
  (check-equal? (starts-with? "" "Hello") #f)
  (check-equal? (starts-with? "Hello" "") #t)
  (check-equal? (starts-with? (list 1 2 3 4 5) (list 1 2)) #t)
  (check-equal? (starts-with? (list 1 2 3 4 5) (list 2 1)) #f)
  (check-equal? (starts-with? #:key (.. string-upcase ->string) "hello there" "Hello") #t)
  (check-equal? (ends-with? "hello there" "there") #t)
  (check-equal? (ends-with? "hello there" "ere") #t)
  (check-equal? (ends-with? "hello there" "ther") #f)
  (check-equal? (ends-with? "hello there" "THERE") #f)
  (check-equal? (ends-with? "" "Hello") #f)
  (check-equal? (ends-with? "Hello" "") #t)
  (check-equal? (ends-with? (list 1 2 3 4 5) (list 5)) #t)
  (check-equal? (ends-with? #:key (.. string-upcase ->string) "hello there" "THERE") #t)
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
  (check-equal? (trim (list 1 2 3 4 5) (curry = 5)) (list 1 2 3 4))
  (check-equal? (trim (list 5 5 1 2 3 4 5 5 5) (curry = 5)) (list 1 2 3 4))
  (check-equal? (->string (trim "   \thello\n  " char-whitespace?)) "hello")
  (check-equal? (->string (trim "   \thello\n  " char-whitespace? #:how-many #f)) "hello")
  (check-equal? (->string (trim "  \thello\n  " char-whitespace? #:how-many 1)) " \thello\n ")
  (check-equal? (->string (trim "   \thello\n   " char-whitespace? #:how-many 2)) " \thello\n ")
  (check-equal? (let-values ([(a b)
                              (split-at (list 1 3 5 2 4) 2)])
                  (->list (map ->list (list a b))))
                (list '(1 3) '(5 2 4)))
  (check-equal? (let-values ([(a b)
                              (split-at "hellothere" 5)])
                  (->list (map ->string (list a b))))
                (list "hello" "there"))
  (check-equal? (let-values ([(a b)
                              (splitf-at (list 1 3 5 2 4) odd?)])
                  (->list (map ->list (list a b))))
                (list '(1 3 5) '(2 4)))
  (check-equal? (->list (add-between (stream 'a 'b 'c) 'and)) '(a and b and c))
  (check-equal? (weave (stream "hi" "there") "\n") "hi\nthere")
  (check-equal? (weave (stream (list 1 2) (list 3 4)) (list 9)) (list 1 2 9 3 4))
  (check-equal? ((weave (list add1 sub1) add1) 5) 6))

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
