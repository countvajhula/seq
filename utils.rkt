#lang racket/base

(require racket/contract
         racket/stream
         racket/match
         racket/generator
         racket/function
         racket/generic
         racket/undefined
         (except-in data/collection
                    foldl
                    foldl/steps
                    append)
         functional-utils
         core-utils
         relation)

(provide every
         takef
         dropf
         split-at
         splitf-at
         starts-with?
         ends-with?
         generator-collection
         generator-collection-gen
         generator-collection?
         gen:producer
         producer-state
         producer?
         generator-cons
         generator-append
         generator-splitf-at
         generator-map
         generator-filter
         in-producer
         add-between
         weave
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

(define (split-at seq pos)
  ;; TODO: make this more efficient
  (values (take pos seq) (drop pos seq)))

(define (splitf-at seq pred)
  ;; TODO: make this more efficient
  (values (takef seq pred) (dropf seq pred)))

(define (slide seq
               [window-size 1])
  ;; TODO: improve; support move-by
  (let ([seqs (for/list ([i (in-range window-size)])
                (drop i seq))])
    (apply map list seqs)))

(define (starts-with? #:key [key #f] str prefix)
  (if (empty? prefix)
      #t
      (and (= #:key key
              (first str)
              (first prefix))
           (starts-with? #:key key
                         (rest str)
                         (rest prefix)))))

(define (ends-with? #:key [key #f] str suffix)
  (starts-with? #:key key
                (reverse str)
                (reverse suffix)))

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

(define-generics producer
  (producer-state producer)
  #:fast-defaults ([generator?
                    (define producer-state generator-state)]))

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

(define (generator-append a b)
  (generator ()
    (let loop ([cur (a)]
               [next (a)])
      (if (= (generator-state a)
             'done)
          (begin (yield cur)
                 (a))
          (begin (yield cur)
                 (loop next (a)))))
    (let loop ([cur (b)]
               [next (b)])
      (if (= (generator-state b)
             'done)
          (begin (yield cur)
                 (b))
          (begin (yield cur)
                 (loop next (b)))))))

(define (in-producer gen [stop undefined] . args)
  (let ([pred (if (undefined? stop)
                  (const #t)
                  (!! (curry = stop)))])
    (takef (build-sequence (apply unthunk gen args))
           pred)))

(define (generator-splitf-at gen pred)
  (splitf-at (in-producer gen (void))
             pred))

(define (generator-map pred gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (yield (pred cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (pred result))))
          (begin (yield (pred cur))
                 (loop next (gen)))))))

(define (generator-filter pred gen)
  (generator ()
    (let loop ([cur (gen)]
               [next (gen)])
      (if (= (generator-state gen)
             'done)
          (begin (when (pred cur)
                   (yield cur))
                 (let ([result (gen)])
                   (unless (void? result)
                     (when (pred result)
                       result))))
          (begin (when (pred cur)
                   (yield cur))
                 (loop next (gen)))))))

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

(define (weave seq sep)
  (fold .. (add-between seq sep)))
