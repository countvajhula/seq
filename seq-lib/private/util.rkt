#lang racket/base

(require syntax/parse/define
         version-case
         (for-syntax racket/base))

(provide define-alias)

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(define-syntax-parse-rule (define-alias alias:id name:id)
  (define-syntax alias (make-rename-transformer #'name)))
