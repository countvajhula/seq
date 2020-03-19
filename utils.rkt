#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         functional-utils
         relation)

(provide every
         takef
         dropf
         splitf-at
         generator-cons
         generator-splitf-at
         in-producer
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

(define (generator-cons v gen)
  (generator ()
    (yield v)
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (yield cur)
                 (gen))
          (begin (yield cur)
                 (loop next (gen)))))))

(struct generator-collection (gen)
  #:transparent
  #:methods gen:collection
  [(define (conj st v)
     (let ([gen (generator-collection-gen st)])
       (generator-collection (generator-cons v gen))))]
  #:property prop:procedure
  (λ (self . args)
    (let ([gen (generator-collection-gen self)])
      (apply gen args))))

(define (generator-splitf-at gen pred)
  (splitf-at (->stream gen) pred))

(define (in-producer gen stop)
  (takef (build-sequence (unthunk gen))
         (!! (curry = stop))))

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
