#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base))

(provide define-alias)

(define-syntax-parse-rule (define-alias alias:id name:id)
  (define-syntax alias (make-rename-transformer #'name)))
