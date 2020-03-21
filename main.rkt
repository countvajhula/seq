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
  (check-equal? (trim (list 1 2 3 4 5) (curry = 5)) (list 1 2 3 4))
  (check-equal? (trim (list 5 5 1 2 3 4 5 5 5) (curry = 5)) (list 1 2 3 4))
  (check-equal? (->string (trim "   \thello\n  " char-whitespace?)) "hello")
  (check-equal? (->string (trim "   \thello\n  " char-whitespace? #:repeat #t)) "hello")
  (check-equal? (->string (trim "  \thello\n  " char-whitespace? #:repeat #f)) " \thello\n ")
  (check-equal? (->list (generator-cons 4 (->generator (list 1 2 3)))) '(4 1 2 3))
  (check-equal? (->list (generator-append (->generator (list 1 2 3)) (->generator (list 4 5 6)))) '(1 2 3 4 5 6))
  (check-equal? (->list (generator-append (->generator (list 1)) (->generator (list 4)))) '(1 4))
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
  (check-equal? (let-values ([(a b)
                              (generator-splitf-at (->generator (list 1 3 5 2 4)) odd?)])
                  (->list (map ->list (list a b))))
                (list '(1 3 5) '(2 4)))
  (check-equal? (->list (add-between (stream 'a 'b 'c) 'and)) '(a and b and c))
  (check-equal? (weave (stream "hi" "there") "\n") "hi\nthere")
  (check-equal? (weave (stream (list 1 2) (list 3 4)) (list 9)) (list 1 2 9 3 4))
  (check-equal? ((weave (list add1 sub1) add1) 5) 6)
  (check-equal? (->list (in-producer (->generator (list 1 2 3 4 (void) 5 6))
                                     (void)))
                '(1 2 3 4))
  (check-equal? (->list (take 7 (in-producer (->generator (list 1 2 3 4 (void) 5 6)))))
                (list 1 2 3 4 (void) 5 6))
  (check-equal? (->list (generator-map add1 (->generator (list 1 2 3))))
                '(2 3 4))
  (check-equal? (->list (generator-filter odd? (->generator (list 1 2 3 4 5 6))))
                '(1 3 5))
  (check-equal? (->list (generator-filter even? (->generator (list 1 2 3 4 5 6))))
                '(2 4 6)))

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
