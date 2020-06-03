#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         scribble/bnf
         racket/sandbox
         @for-label[(except-in racket
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

These utilities build on top of the @other-doc['(lib "scribblings/data/collection/collections.scrbl")] foundation to provide a broad range of general-purpose utilities that work on all sequences.

Some of these interfaces are either implementations of or are inspired by the Scheme @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html"]{specifications} for @hyperlink["https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html"]{list utilities}, while others are similar in spirit. Many operations we may desire to perform on sequences stem from simple intuitions, so an attempt has been made to adhere to naming conventions and categories that might map to these intuitions.

@table-of-contents[]

@section{Naming Conventions}

These naming conventions are intended to minimize the need to look up documentation while working with sequences.

While some of the provided sequence utilities have standard names familiar from string or list contexts, others take their name from the Scheme specification or are borrowed from other functional languages such as Haskell. When the utilities don't take their name from one of these sources, they instead have a "canonical" name that is approximately expressed as:

@(let ([open @litchar{(}]
       [close @litchar{)}]
       [hyphen @litchar{-}])
     @BNF[(list @nonterm{expr}
                @nonterm{verb phrase}
                @nonterm{special phrase})
          (list @nonterm{verb phrase}
                @BNF-seq[open @nonterm{verb}
                              @kleenestar[@nonterm{args}]
                              @nonterm{noun}
                         close]
                @BNF-seq[open @nonterm{verb} hyphen @nonterm{modifier}
                              @kleenestar[@nonterm{args}]
                              @nonterm{noun}
                         close])
          (list @nonterm{verb}
                @litchar{take}
                @litchar{drop}
                @litchar{cut}
                @litchar{trim}
                @litchar{find}
                @litchar{remove}
                @litchar{choose}
                @litchar{index}
                @litchar{zip})
          (list @nonterm{modifier}
                @litchar{while}
                @litchar{until}
                @litchar{when}
                @litchar{if}
                @litchar{unless}
                @litchar{at}
                @litchar{where}
                @litchar{with}
                @litchar{by})
          (list @nonterm{special phrase}
                @BNF-seq[open @nonterm{special operation}
                              @kleenestar[@nonterm{args}]
                              @nonterm{noun}
                         close])
          (list @nonterm{special operation}
                @litchar{every}
                @litchar{exists}
                @litchar{for-all}
                @litchar{join}
                @litchar{add-between}
                @litchar{wrap-each}
                @litchar{slide}
                @litchar{interleave}
                @litchar{choose}
                @litchar{deduplicate}
                @litchar{cascade}
                @litchar{weave})
          (list @nonterm{args}
                @elem{any parameters for the operation to be performed})
          (list @nonterm{noun}
                @elem{@litchar{sequence?}})])

Whenever a canonical name is used for a well-known interface, the more common name is also usually provided as an alias. Not every interface here corresponds neatly to a naming convention, but in cases where they do, verbs and suffixes have the following meanings:

@subsection{Suffixes}

@itemize[
  @item{@bold{Undecorated verbs} usually check for equality. E.g. @racket[trim] removes the specified elements at the head and tail of a sequence (if present).}
  @item{@bold{-where} indicates a specific place with respect to the contents of the sequence, for instance @racket[cut-where] splits the input sequence at a particular (the first) point where a given predicate evaluates to true.}
  @item{@bold{-at} indicates a specific place in the sequence @emph{by position}, for instance @racket[cut-at] splits the input sequence at the indicated index.}
  @item{@bold{-when} indicates a sequence-spanning condition -- @racket[take-when] takes @emph{all} elements in the input sequence for which a predicate holds (more commonly known as @racket[filter]).}
  @item{@bold{-while} indicates a running condition -- e.g. @racket[take-while] takes @emph{as long as} a predicate holds, and then stops at the point where it fails.}
  @item{@bold{-until} indicates a running condition, the negation of "-while" -- e.g. @racket[take-until] takes as long as a predicate @emph{does not hold}, and then stops at the point where it returns true.}
  @item{@bold{-by} indicates operations that deal in terms of lengths rather than the actual contents of the sequence. For instance, @racket[trim-by] removes a certain @emph{number} of elements at the head and tail of the sequence.}
  @item{@bold{-with} indicates a function to be used as part of the operation. For instance, @racket[zip-with] "zips" sequences together by using a provided function to combine partnered elements.}
  @item{@bold{-if} indicates a sequence-spanning condition. @racket[trim-if] removes elements at the head and tail of a sequence @emph{if} some condition is met.}
  @item{@bold{-unless} is a sequence-spanning condition, the negation of "-if". E.g. @racket[trim-unless] removes elements at the head and tail of a sequence @emph{unless} some condition is met. Note that in general, "-unless" is avoided in favor of simply using the opposite verbs. For instance, in lieu of @racket[take-unless], there's @racket[drop-when].}
]

@subsection{Verbs}

@itemize[
  @item{@bold{take} and @bold{drop} refer to elements, hence they return a sequence containing the relevant @emph{elements} in the original sequence.}
  @item{@bold{cut} and @bold{infix} refer to subsequences, hence they either accept or return subsequences of the original sequence conforming to the query. E.g. @racket[find-infix] searches for a @emph{subsequence} in the input, rather than an individual element. @racket[cut-when] returns a sequence of subsequences cut from the original. Most string-related operations are of this kind.}
]

@section{APIs}

@subsection{Specific Element}

@defproc[(find [pred procedure?]
               [seq sequence?]
               ...)
         sequence?]{

 Find the first element in @racket[seq] that fulfills @racket[pred]. If no item fulfills the predicate, then the result is @racket[#f].

@examples[
    #:eval eval-for-docs
    (find number? (list "cherry" 'banana 10 30))
    (find positive? (list -1 -2 -1 2 3))
    (find (curry prefix-of? "ap") (list "banana" "apple" "apricot"))
  ]
}


@subsection{Index and Length-based}

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

@subsection{Infix}

@defproc[(cut-when [pred procedure?]
                     [seq sequence?])
         sequence?]{

 Cut a subsequence from @racket[seq] at each point where @racket[pred] is satisfied.

@examples[
    #:eval eval-for-docs
    (->list (cut-when (curry = #\space) "hello there old friend"))
    (->list (map ->list (cut-when negative? (list -1 4 1 -3 2 -5 3 7))))
  ]
}

@subsection{Predicates}

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

@subsection{Filtering}

@defproc[(take-when [pred procedure?]
                    [seq sequence?])
         sequence?]{

 Select all elements from @racket[seq] that satisfy @racket[pred]. An alias for @racket[filter].

@examples[
    #:eval eval-for-docs
    (->list (take-when positive? (list 1 -4 -1 3)))
    (->list (take-when (curry prefix-of? "ap") (list "banana" "apple" "apricot" "cherry")))
  ]
}

@deftogether[(
@defproc[(take-while [pred procedure?]
                     [seq sequence?])
         sequence?]
@defproc[(drop-while [pred procedure?]
                     [seq sequence?])
         sequence?]
)]{

 Select (take) or exclude (drop) elements from @racket[seq] as long as they satisfy @racket[pred], stopping at the first one that fails to.

@examples[
    #:eval eval-for-docs
    (->list (take-while positive? (list 1 2 -4 -12 3)))
    (->list (drop-while positive? (list 1 2 -4 -12 3)))
    (->list (take-while positive? (list -1 3 2 4 -12)))
    (->list (drop-while positive? (list -1 3 2 4 -12)))
    (->list (take-while (curry prefix-of? "ap") (list "apple" "banana" "apricot" "cherry")))
    (->list (drop-while (curry prefix-of? "ap") (list "apple" "banana" "apricot" "cherry")))
  ]
}

@deftogether[(
@defproc[(take-until [pred procedure?]
                     [seq sequence?])
         sequence?]
@defproc[(drop-until [pred procedure?]
                     [seq sequence?])
         sequence?]
)]{
 Select (take) or exclude (drop) elements from @racket[seq] as long as they @emph{do not} satisfy @racket[pred], stopping at the first one that succeeds.

@examples[
    #:eval eval-for-docs
    (->list (take-until positive? (list -1 -2 3 2 -4)))
    (->list (drop-until positive? (list -1 -2 3 2 -4)))
    (->list (take-until positive? (list 1 3 2 -4)))
    (->list (drop-until positive? (list 1 3 2 -4)))
    (->list (take-until (curry prefix-of? "ap") (list "banana" "apple" "apricot" "cherry")))
    (->list (drop-until (curry prefix-of? "ap") (list "banana" "apple" "apricot" "cherry")))
  ]
}

@subsection{Composing}

@deftogether[(
@defproc[(zip [seq sequence?]
              ...)
         (sequenceof list?)]
@defproc[(zip-with [op procedure?]
                   [seq sequence?]
                   ...)
         any/c]
@defproc[(unzip [seq sequence?])
         (sequenceof list?)]
@defproc[(unzip-with [op procedure?]
                     [seq sequence?])
         any/c]
)]{

 @racket[zip-with] merges the input sequences using the provided operation @racket[op]. Equivalent to Haskell's @hyperlink["http://zvon.org/other/haskell/Outputprelude/zipWith_f.html"]{zipWith}.

 @racket[zip] is equivalent to @racket[zip-with list].

 @racket[zip] is its own inverse, so applying it twice is essentially equivalent to doing nothing. Still, @racket[unzip-with] and @racket[unzip] are provided with a slightly different signature -- accepting a single sequence rather than an arbitrary number of sequences, making it convenient to apply to a result already derived by using @racket[zip]. @racket[unzip] is equivalent to @racket[(curry apply zip)].

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
    (->list (unzip (zip (list 'a 'b 'c) (list 1 2 3))))
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

@subsection{Permuting}

@deftogether[(
@defproc[(rotate-left [n exact-nonnegative-integer?]
                      [seq sequence?])
         sequence?]
@defproc[(rotate-right [n exact-nonnegative-integer?]
                       [seq sequence?])
         sequence?]
)]{
 Derive a new sequence by shifting the elements of @racket[seq] to the left or to the right, wrapping around the tail of the list.

@examples[
    #:eval eval-for-docs
    (->list (rotate-left 1 (range 1 8)))
    (->list (rotate-right 1 (range 1 8)))
    (->list (rotate-left 3 (range 1 8)))
    (->list (rotate-right 3 (range 1 8)))
    (->string (rotate-left 2 "avocado"))
    (->string (rotate-right 2 "avocado"))
  ]
}
