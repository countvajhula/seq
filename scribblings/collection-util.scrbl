#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[(except-in racket
                               split-at
                               add-between
                               index-of
                               sequence?
                               remove)
                    (prefix-in r: relation)
                    (only-in relation ->list)
                    collection-util
                    (prefix-in d: data/collection)
                    (only-in data/collection
                             sequence?
                             sequenceof
                             repeat
                             subsequence
                             naturals)]]

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

Many sequence utilities have a central verb - for instance, @racketlink[d:take]{take} or @racket[split]. In all such cases, where applicable, suffixes have the following meanings:

@itemize[
  @item{@bold{Undecorated verbs} usually check for equality. E.g. @racket[trim] removes the specified elements at the head and tail of a sequence (if present).}
  @item{@bold{-where} indicates a specific place in the sequence, for instance @racket[split-where] splits the input sequence at a particular (the first) point where a given predicate evaluates to true.}
  @item{@bold{-when} indicates a sequence-spanning condition -- @racket[take-when] takes @emph{all} elements in the input sequence for which a predicate holds (more commonly known as @racket[filter]).}
  @item{@bold{-while} indicates a running condition -- e.g. @racket[take-while] takes @emph{as long as} a predicate holds, and then stops at the point where it fails.}
  @item{@bold{-until} indicates a running condition, the negation of "-while" -- e.g. @racket[take-until] takes as long as a predicate @emph{does not hold}, and then stops at the point where it returns true.}
  @item{@bold{-if} indicates a sequence-spanning condition. @racket[trim-if] removes elements at the head and tail of a sequence @emph{if} some condition is met.}
  @item{@bold{-unless} is a sequence-spanning condition, the negation of "-if". E.g. @racket[trim-unless] removes elements at the head and tail of a sequence @emph{unless} some condition is met. Note that in general, "-unless" is avoided in favor of simply using the opposite verbs. For instance, in lieu of @racket[take-unless], there's @racket[drop-when].}
  @item{@bold{-by} indicates operations that deal in terms of lengths rather than the actual contents of the sequence. For instance, @racket[trim-by] removes a certain @emph{number} of elements at the head and tail of the sequence.}
]

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

@defproc[(exists [pred (-> any/c boolean?)]
                 [seq sequence?]
                 ...)
         boolean?]{

 Similar to @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html?q=for-all#node_idx_206"]{exists} but generalized to all sequences rather than only lists, this checks if @emph{any} of the sequence values fulfill a provided predicate. @racket[pred] must accept a number of arguments equal to the number of provided sequences @racket[seq]. This is an alias for @racketlink[d:ormap]{ormap}.

@examples[
    #:eval eval-for-docs
    (exists positive? (list -1 -3 0 -2 -5))
    (exists positive? (list -1 -3 0 2 -5))
    (exists < (list 1 2 3 4 5) (list 1 0 2 2 7))
  ]
}

@defproc[(for-all [pred (-> any/c boolean?)]
                  [seq sequence?]
                  ...)
         boolean?]{

 Similar to @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html?q=for-all#node_idx_204"]{for-all} but generalized to all sequences rather than only lists, this checks if @emph{all} of the sequence values fulfill a provided predicate. @racket[pred] must accept a number of arguments equal to the number of provided sequences @racket[seq]. This is an alias for @racketlink[d:andmap]{andmap}.

@examples[
    #:eval eval-for-docs
    (for-all positive? (list -1 3 0 2 5))
    (for-all positive? (list 1 3 2 2 5))
    (for-all < (list 1 2 3 4 5) (list 2 3 4 5 6))
  ]
}

@deftogether[(
@defproc[(zip-with [op procedure?]
                   [seq sequence?]
                   ...)
         any/c]
@defproc[(zip [seq sequence?]
              ...)
         (sequenceof list?)]
)]{

 @racket[zip-with] merges the input sequences using the provided operation @racket[op]. Equivalent to Haskell's @hyperlink["http://zvon.org/other/haskell/Outputprelude/zipWith_f.html"]{zipWith}. @racket[zip] is equivalent to @racket[zip-with list].

@examples[
    #:eval eval-for-docs
    (->list (zip (list 'a 'b 'c) (list 1 2 3 4 5)))
    (->list (zip-with + (list 1 2 3) (list 3 2 1)))
    (->list (zip-with expt (repeat 5) (range 10)))
    (->list (zip-with (lambda (x y)
                        (+ (* 2 x)
                           y))
                      (range 1 5)
                      (range 5 9)))
  ]
}

@defproc[(choose [pred procedure?]
                 [seq sequence?]
                 ...)
         sequence?]{

 Lazily choose a single item from each of the input sequences -- the first one that fulfills the choice predicate @racket[pred]. The result is a sequence containing as many values as the number of input sequences. If no item in a particular sequence fulfills the choice predicate, then the corresponding element in the resulting sequence is @racket[#f].

@examples[
    #:eval eval-for-docs
    (->list (choose number? (list 10 "left shoe" 30) (list "right shoe" 15 15) (list "sock" -55 7)))
    (->list (choose positive? (list -1 -2 1 2) (list -5 3 -2) (list 5 2 -1)))
    (->list (choose (curry prefix-of? "ap") (list "banana" "apple" "apricot") (list "dog" "cat" "ape")))
  ]
}
