#lang racket/base

(provide (all-from-out seq/api))

(require (except-in seq/api
                    ;; these are provided only so they can be tested
                    ;; outside the lib package and avoid introducing
                    ;; testing dependencies to it. They aren't meant
                    ;; to be used by (other) clients of the library.
                    seq-test:annotate-result
                    seq-test:annotate-result-naively))
