#lang racket/base

(require racket/set
         racket/list
         arguments
         (for-syntax racket/base
                     arguments)
         (only-in data/collection
                  sequence->list
                  apply
                  known-finite?)
         relation/type
         (prefix-in s: "base.rkt")
         syntax/parse/define)

(provide (all-from-out "base.rkt"))

;; (define/arguments (wrap-if fn args)
;;   (let ([result (apply/arguments fn args)])
;;     ))

#|
VAO
VAAO
O
V-OS
no-change

take-when
(define take-when (iso b:take-when))
(iso fn) => (lambda/arguments args

|#

(define-syntax-parser iso
  [(_ intf)
   #'(lambda/arguments args
                       (let ([seq (first (arguments-positional args))])
                         (let ([result (intf seq)])
                           (cond [(and (list? seq) (known-finite? result)) (->list result)]
                                 [(and (string? seq) (known-finite? result)) (->string result)]
                                 [(and (vector? seq) (known-finite? result)) (->vector result)]
                                 [(and (bytes? seq) (known-finite? result)) (->bytes result)]
                                 [(and (set? seq) (known-finite? result)) (->set result)]
                                 [(and (hash? seq) (known-finite? result)) (->hash result)]
                                 [else result]))))]
  [(_ intf (~datum VAO))
   #'(lambda/arguments args
                       (let ([arg (first (arguments-positional args))]
                             [seq (second (arguments-positional args))])
                         (let ([result (intf arg seq)])
                           (cond [(and (list? seq) (known-finite? result)) (->list result)]
                                 [(and (string? seq) (known-finite? result)) (->string result)]
                                 [(and (vector? seq) (known-finite? result)) (->vector result)]
                                 [(and (bytes? seq) (known-finite? result)) (->bytes result)]
                                 [(and (set? seq) (known-finite? result)) (->set result)]
                                 [(and (hash? seq) (known-finite? result)) (->hash result)]
                                 [else result]))))]
  [(_ intf (~datum VAAO))
   #'(lambda/arguments args
                       (let ([arg1 (first (arguments-positional args))]
                             [arg2 (second (arguments-positional args))]
                             [seq (third (arguments-positional args))])
                         (let ([result (intf arg1 arg2 seq)])
                           (cond [(and (list? seq) (known-finite? result)) (->list result)]
                                 [(and (string? seq) (known-finite? result)) (->string result)]
                                 [(and (vector? seq) (known-finite? result)) (->vector result)]
                                 [(and (bytes? seq) (known-finite? result)) (->bytes result)]
                                 [(and (set? seq) (known-finite? result)) (->set result)]
                                 [(and (hash? seq) (known-finite? result)) (->hash result)]
                                 [else result]))))]
  [(_ intf (~datum V-OS))
   #'(lambda/arguments args
                       (let* ([seqs (arguments-positional args)]
                              [seq (and (not (empty? seqs)) (first seqs))])
                         (let ([result (apply intf seqs)])
                           (cond [(and seq (list? seq) (known-finite? result)) (->list result)]
                                 [(and seq (string? seq) (known-finite? result)) (->string result)]
                                 [(and seq (vector? seq) (known-finite? result)) (->vector result)]
                                 [(and seq (bytes? seq) (known-finite? result)) (->bytes result)]
                                 [(and seq (set? seq) (known-finite? result)) (->set result)]
                                 [(and seq (hash? seq) (known-finite? result)) (->hash result)]
                                 [else result]))))])

(define take-when (iso s:take-when VAO))

(define prefix (iso s:prefix VAO))

(define suffix-at (iso s:suffix-at VAO))

(define infix (iso s:infix VAAO))

(define infix-at (iso s:infix-at VAAO))

;; need:
;; 1. return sequence of appropriate known-finite? in all interfaces?
;; -> but can probbaly move on for now ?
(define by (iso s:by VAO))

(define init (iso s:init))

(define exists s:exists)

(define for-all s:for-all)

(define zip-with (iso s:zip-with V-OS))

;; (define zip (iso s:zip VAO))
;; (define unzip-with (iso s:unzip-with VAO))
;; (define unzip (iso s:unzip VAO))
;; (define find (iso s:find VAO))
;; (define index-where (iso s:index-where VAO))
;; (define choose (iso s:choose VAO))
;; (define suffix (iso s:suffix VAO))
;; (define take-while (iso s:take-while VAO))
;; (define drop-while (iso s:drop-while VAO))
;; (define take-until (iso s:take-until VAO))
;; (define drop-until (iso s:drop-until VAO))
;; (define cut-when (iso s:cut-when VAO))
;; (define cut (iso s:cut VAO))
;; (define cut-at (iso s:cut-at VAO))
;; (define cut-where (iso s:cut-where VAO))
;; (define cut-by (iso s:cut-by VAO))
;; (define cut-with (iso s:cut-with VAO))
;; (define truncate (iso s:truncate VAO))
;; (define rotate-left (iso s:rotate-left VAO))
;; (define rotate-right (iso s:rotate-right VAO))
;; (define rotate (iso s:rotate VAO))
;; (define rotations (iso s:rotations VAO))
;; (define deduplicate (iso s:deduplicate VAO))
;; (define multiples (iso s:multiples VAO))
;; (define powers (iso s:powers VAO))
;; (define iterate (iso s:iterate VAO))
;; (define suffixes (iso s:suffixes VAO))
;; (define prefixes (iso s:prefixes VAO))
;; (define infixes (iso s:infixes VAO))
;; (define prefix? (iso s:prefix? VAO))
;; (define starts-with? (iso s:starts-with? VAO))
;; (define suffix? (iso s:suffix? VAO))
;; (define ends-with? (iso s:ends-with? VAO))
;; (define find-infix (iso s:find-infix VAO))
;; (define replace-infix (iso s:replace-infix VAO))
;; (define infix? (iso s:infix? VAO))
;; (define contains? (iso s:contains? VAO))
;; (define trim-if (iso s:trim-if VAO))
;; (define trim (iso s:trim VAO))
;; (define trim-by (iso s:trim-by VAO))
;; (define index-of (iso s:index-of VAO))
;; (define index (iso s:index VAO))
;; (define remove (iso s:remove VAO))
;; (define remove-at (iso s:remove-at VAO))
;; (define drop-when (iso s:drop-when VAO))
;; (define intersperse (iso s:intersperse VAO))
;; (define add-between (iso s:add-between VAO))
;; (define join-with (iso s:join-with VAO))
;; (define wrap-each (iso s:wrap-each VAO))
;; (define weave (iso s:weave VAO))
;; (define interleave (iso s:interleave VAO))
