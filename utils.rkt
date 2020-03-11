#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         (except-in data/collection
                    foldl
                    foldl/steps)
         relation)

(provide every
         takef
         dropf
         splitf-at
         generator-cons
         generator-splitf-at
         add-between
         string-join
         (contract-out
          [: (collection? any/c . -> . collection?)]))

(define : conj)

(define (every cnt seq)
  (if (empty? seq)
      empty-stream
      (let ([head (first seq)]
            [tail (with-handlers
                    ([exn:fail?
                      (λ (exn)
                        empty-stream)])
                    (drop cnt seq))])
        (stream-cons head (every cnt tail)))))

(define (takef seq pred)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (takef vs pred))
            null))))

(define (dropf seq pred)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (dropf vs pred)
            seq))))

(define (splitf-at seq pred)
  ;; TODO: make this more efficient
  (values (takef seq pred) (dropf seq pred)))

(define (slide seq
               [window-size 1])
  ;; TODO: improve; support move-by
  (let ([seqs (for/list ([i (in-range window-size)])
                (drop i seq))])
    (apply map list seqs)))

(define (generator-cons v gen [stop (void)])
  (generator ()
    (yield v)
    (let loop ([val (gen)])
      (unless (= val stop)
        (yield val)
        (loop (gen))))))

(define (generator-splitf-at gen pred)
  (splitf-at (->stream gen) pred))

(define (add-between seq sep)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (empty? vs)
            (stream v)
            (stream-cons v
                         (stream-cons sep
                                      (add-between vs sep)))))))

(define (string-join seq sep)
  (fold .. (add-between seq sep) ""))
