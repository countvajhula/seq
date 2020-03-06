#lang racket

(require racket/stream
         racket/match
         (except-in data/collection
                    foldl)
         relation)

(provide every
         takef
         dropf
         splitf-at
         generator-cons
         generator-splitf-at
         add-between
         string-join)

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

(define (generator-cons v gen)
  (let* ([old-stream (->stream gen)]
         [new-stream (conj old-stream v)])
    (->generator new-stream)))

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
  (string-trim (fold .. (map (curryr .. sep)
                             seq))
               sep))
