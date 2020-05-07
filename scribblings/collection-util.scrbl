#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[(except-in racket/base
                               length
                               reverse
                               apply
                               remove
                               map
                               filter
                               append
                               sequence?
                               for-each
                               andmap
                               ormap)
                    collection-util
                    (except-in data/collection
                               index-of
                               foldl
                               foldl/steps)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation
                                           (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps)
								 		   collection-util
                                 		   racket/set
                                 		   racket/stream))))

@title{Collection Utilities}
@author{Siddhartha Kasivajhula}

@defmodule[collection-util]

Standard and general-purpose collection utilities.

These utilities build on top of the foundation for generic collections and provide a broad range of general-purpose utilities that work on all sequences.

Some of these interfaces are either implementations of or are inspired by the Scheme specifications for list utilities, while others are similar in spirit. Many operations we may desire to perform on sequences stem from simple intuitions, so an attempt has been made to adhere to naming conventions that might map to these intuitions in a minimal way.

@table-of-contents[]

@section{Naming Conventions}

Many sequence utilities have a central verb - for instance, @racket[take] or @racket[split]. In all such cases, where applicable, suffixes have the following meanings:

Undecorated verbs usually check for equality. E.g. @racket[trim] removes the specified elements at the head and tail of a sequence (if present).
"-where" indicates a specific place in the sequence, for instance @racket[split-where] splits the input sequence at a particular (the first) point where a given predicate evaluates to true.
"-when" indicates a sequence-spanning condition -- @racket[take-when] takes @emph{all} elements in the input sequence for which a predicate holds (more commonly known as @racket[filter]).
"-while" indicates a running condition -- e.g. @racket[take-while] takes @emph{as long as} a predicate holds, and then stops at the point where it fails.
"-until" indicates a running condition, the negation of "-while" -- e.g. @racket[take-until] takes as long as a predicate @emph{does not hold}, and then stops at the point where it returns true.
"-if" indicates a sequence-spanning condition. @racket[trim-if] removes elements at the head and tail of a sequence @emph{if} some condition is met.
"-unless" is a sequence-spanning condition, the negation of "-if". E.g. @racket[trim-unless] removes elements at the head and tail of a sequence @emph{unless} some condition is met. Note that in general, "-unless" is avoided in favor of simply using the opposite verbs. For instance, in lieu of @racket[take-unless], there's @racket[drop-when].
"-length" indicates operations that deal in terms of lengths rather than the actual contents of the sequence. For instance, @racket[trim-length] removes a certain @emph{number} of elements at the head and tail of the sequence.

@section{APIs}

@defproc[(every [n exact-nonnegative-integer?]
                [seq sequence?])
         sequence?]{

 A sequence containing every @racket[n]'th element in the input sequence.

@examples[
    #:eval eval-for-docs
    (->list (take 10 (every 5 (naturals))))
    (->list (take 10 (every 2 (naturals))))
    (->list (take 10 (every 2 (naturals 1))))
    (->list (take 10 (every 7 (drop 100 (naturals)))))
    (->list (every 3 (subsequence (naturals) 10 20)))
    (->list (every 3 #(1 2 3 4 5 6 7 8 9 10)))
  ]
}
