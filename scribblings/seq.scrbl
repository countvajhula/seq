#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         scribble/bnf
         racket/sandbox
         @for-label[(except-in racket
                               add-between
                               index-of
                               index-where
                               range
                               map
                               filter
                               reverse
                               rest
                               take
                               drop
                               sequence?
                               truncate
                               init
                               prefix
                               remove)
                    (only-in racket
                             (add-between b:add-between)
                             (range b:range))
                    (prefix-in r: relation)
                    (only-in relation
                             ->list
                             ->string
                             ->number
                             gen:appendable
                             join
                             onto
                             ~
                             ^
                             arg
                             flip
                             flip*
                             power
                             comparable?)
                    seq
                    (prefix-in d: data/collection)
                    (only-in data/collection
                             conj
                             conj*
                             extend
                             gen:collection
                             gen:sequence
                             sequence?
                             collection?
                             sequenceof
                             repeat
                             cycle
                             subsequence
                             subsequence*
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
                                                      index-where
                                                      range
                                                      nth
                                                      set-nth
                                                      map
                                                      filter
                                                      reverse
                                                      rest
                                                      take
                                                      drop
                                                      foldl
                                                      foldl/steps)
                                           seq/iso
                                           racket/set
                                           racket/math
                                           racket/stream))))

@title{Seq: A Sequence Library}
@author{Siddhartha Kasivajhula}

@defmodule[seq]

Standard and general-purpose sequence utilities.

This library builds on top of the @other-doc['(lib "scribblings/data/collection/collections.scrbl")] foundation to provide a broad range of general-purpose utilities that work on all sequences. It also leverages @other-doc['(lib "relation/scribblings/relation.scrbl")] to provide an optional "isomorphic" layer that ensures symmetry of input and output types while using these interfaces.

Some of these interfaces are either implementations of or are inspired by the Scheme @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html"]{specifications} for @hyperlink["https://docs.racket-lang.org/srfi/srfi-std/srfi-1.html"]{list utilities}, while others are similar in spirit. An attempt has been made to adhere to intuitive naming conventions and categories to minimize the need to lookup documentation and support the ability to guess the name of an unknown function rather than learn these (numerous) names purely through familiarity.

@table-of-contents[]

@section{Modules}

@defmodule*/no-declare[(seq/base)]

The core set of APIs, exhibiting functional, generic, and lazy semantics (like the interfaces in @racket[data/collection]). This is an internal module and generally should not be used directly.

@defmodule*/no-declare[(seq/api)]

The default module that is imported via @racket[(require seq)]. It simply annotates the interfaces in @racket[seq/base], as well as a few from @racket[data/collection], with known finiteness information, which may be leveraged by applications for whatever purpose. In particular, this information is used in the @racket[seq/iso] layer to determine whether preserving type symmetry is possible. Once again, this is an internal module and should not be used directly -- just implicitly via @racket[(require seq)].

@defmodule*/no-declare[(seq/iso)]

This module provides all of the APIs from @racket[seq/api] in "isomorphic" form, so that output types will match input types where it makes sense. In general, therefore, the interfaces in this module are @emph{not} lazy.

In particular, output types will match input types for all finite @techlink[#:doc '(lib "scribblings/data/collection/collections.scrbl") #:key "generic sequence"]{sequences} that are either (a) a known built-in type such as a @tech/reference{list}, or (b) a custom type implementing both @racket[gen:collection] as well as @racket[gen:appendable] (in addition to @racket[gen:sequence]). Note that in order for these isomorphic APIs to function correctly for custom types, the implementation of @racket[extend] for @racket[gen:collection] in the custom type must preserve the order of elements (for example, unlike the @racketlink[extend]{default implementation for lists}, which reverses the order of elements).

@section{Naming Conventions}

These naming conventions are intended to minimize the need to look up documentation while working with sequences.

While some of the provided sequence utilities have standard names familiar from string or list contexts, others take their name from the Scheme specification or are borrowed from other functional languages such as Haskell. When the utilities don't take their name from one of these sources, they instead have a formulaic name that is approximately expressed as:

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
                @litchar{prefix}
                @litchar{suffix}
                @litchar{cut}
                @litchar{trim}
                @litchar{find}
                @litchar{remove}
                @litchar{choose}
                @litchar{index}
                @litchar{zip}
                @litchar{unzip})
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
                @litchar{by}
                @litchar{exists}
                @litchar{for-all}
                @litchar{join-with}
                @litchar{intersperse}
                @litchar{add-between}
                @litchar{wrap-each}
                @litchar{interleave}
                @litchar{choose}
                @litchar{deduplicate}
                @litchar{prefix?}
                @litchar{starts-with?}
                @litchar{suffix?}
                @litchar{ends-with?}
                @litchar{infix?}
                @litchar{contains?}
                @litchar{prefix}
                @litchar{suffix}
                @litchar{suffix-at}
                @litchar{prefixes}
                @litchar{suffixes}
                @litchar{infixes}
                @litchar{weave}
                @litchar{rotate-left}
                @litchar{rotate-right}
                @litchar{multiples}
                @litchar{powers})
          (list @nonterm{args}
                @elem{any parameters for the operation to be performed})
          (list @nonterm{noun}
                @elem{@litchar{sequence?}})])

Whenever a formulaic name is used for a well-known interface, the more common name is also usually provided as an alias. Not every interface here corresponds neatly to a naming convention, but in cases where they do, verbs and suffixes have the following meanings:

@subsection{Verbs}

@itemize[
  @item{@bold{take} and @bold{drop} refer to elements, hence they return a sequence containing the relevant @emph{elements} in the original sequence.}
  @item{@bold{cut} and @bold{infix} refer to subsequences, hence they either accept or return subsequences of the original sequence conforming to the query. E.g. @racket[find-infix] searches for a @emph{subsequence} in the input, rather than an individual element. @racket[cut-when] returns a sequence of subsequences cut from the original. Most string-related operations are of this kind.}
  @item{@bold{find} and @bold{remove} without additional qualification operate on individual elements.}
]

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

@section{APIs}

The API documentation here applies to all of the @seclink["Modules" #:doc '(lib "seq/scribblings/seq.scrbl")]{seq modules}, but the examples use @racket[seq/iso] for convenience.

@subsection{Index and Length-based}

Reason in terms of gestalt properties of sequences, such as index and length, as opposed to their contents.

@defproc[(by [n exact-nonnegative-integer?]
             [seq sequence?])
         sequence?]{

 A sequence containing every @racket[n]'th element in the input sequence.

@examples[
    #:eval eval-for-docs
    (->list (take 10 (by 5 (naturals))))
    (->list (take 10 (by 2 (naturals))))
    (->list (take 10 (by 2 (naturals 1))))
    (->list (take 10 (by 7 (drop 100 (naturals)))))
    (->list (by 3 (subsequence (naturals) 10 20)))
    (->list (by 3 #(1 2 3 4 5 6 7 8 9 10)))
  ]
}

@defproc[(rest [seq sequence?])
         sequence?]{

 Identical to @racketlink[d:rest]{@racket[rest]} from @racket[data/collection], except that it includes additional runtime annotations to support isomorphic behavior.

@examples[
    #:eval eval-for-docs
    (rest (list 1 2 3))
    (rest "apple")
    (->list (take 5 (rest (naturals))))
  ]
}

@defproc[(init [seq sequence?])
         sequence?]{

 The opposite of @racket[rest], this produces a sequence containing all of the elements of @racket[seq] @emph{except} the last one. Like @racket[rest], this sequence is lazily generated.

@examples[
    #:eval eval-for-docs
    (init (list 1 2 3))
    (init "apple")
    (->list (take 5 (init (naturals))))
  ]
}

@deftogether[(
@defproc[(take [n exact-nonnegative-integer?]
               [seq sequence?])
         sequence?]
@defproc[(prefix [n exact-nonnegative-integer?]
                 [seq sequence?])
         sequence?]
@defproc[(suffix [n exact-nonnegative-integer?]
                 [seq sequence?])
         sequence?]
@defproc[(drop [n exact-nonnegative-integer?]
               [seq sequence?])
         sequence?]
@defproc[(suffix-at [n exact-nonnegative-integer?]
                    [seq sequence?])
         sequence?]
)]{
  @racket[prefix] returns the first @racket[n] elements of @racket[seq], i.e. a prefix of length @racket[n]; it is an alias for @racket[take]. @racket[suffix] analogously returns the last @racket[n] elements of @racket[seq], i.e. a suffix of length @racket[n]. @racket[suffix-at] is an alias for @racket[drop], returning the suffix at the @emph{index} @racket[n]. @racket[take] and @racket[drop] are identical to @racketlink[d:take]{@racket[take]} and @racketlink[d:drop]{@racket[drop]} from @racket[data/collection] except that they include additional runtime annotations to support isomorphic behavior.

@examples[
    #:eval eval-for-docs
    (prefix 2 "apricot")
    (suffix 2 "apricot")
    (suffix-at 2 "apricot")
    (~ (prefix 2 "apricot") (suffix-at 2 "apricot"))
    (prefix 2 (list "banana" "apple" "apricot" "cherry" "avocado"))
    (suffix 3 (list 1 2 3 4 5 6 7 8 9))
  ]
}

@deftogether[(
@defproc[(infix [start exact-nonnegative-integer?]
                [length exact-nonnegative-integer?]
                [seq sequence?])
         sequence?]
@defproc[(infix-at [start exact-nonnegative-integer?]
                   [end exact-nonnegative-integer?]
                   [seq sequence?])
         sequence?]
)]{
  @racket[infix] and @racket[infix-at] compose @racket[prefix] and @racket[suffix] to extract subsequences of seq. @racket[infix] expects a @racket[start] index along with the length of the subsequence to extract, while @racket[infix-at] expects @racket[start] and @racket[end] positions identifying the subsequence to be extracted. @racket[infix] is essentially an alias for @racket[subsequence*], while @racket[infix-at] is an alias for @racket[subsequence].

@examples[
    #:eval eval-for-docs
    (infix 4 5 "the quick brown fox")
    (infix-at 4 9 "the quick brown fox")
    (infix 10 5 "the quick brown fox")
    (infix-at 10 15 "the quick brown fox")
    (->list (infix 64 5 (range 100)))
    (->list (infix-at 64 69 (range 100)))
  ]
}

@deftogether[(
@defproc[(index-of [#:key key procedure? #f]
                   [elem any/c]
                   [seq sequence?])
         exact-nonnegative-integer?]
@defproc[(index [#:key key procedure? #f]
                [elem any/c]
                [seq sequence?])
         exact-nonnegative-integer?]
)]{

 The index of @racket[elem] in @racket[seq], or @racket[#f] if it doesn't exist. If @racket[key] is provided, it is used in the @racketlink[r:=]{@racket[=]} comparison to find @racket[elem]. @racket[index] is an alias of @racket[index-of].

@examples[
    #:eval eval-for-docs
    (index-of 3 (list 1 2 3 4 5))
    (index-of #:key string-upcase "cherry" (list "Apple" "CHERry" "BaNaNa"))
    (index-of " " "The quick brown fox")
  ]
}

@defproc[(index-where [pred procedure?]
                      [seq sequence?]
					  ...)
         sequence?]{

 The first index where @racket[pred] is true. The predicate @racket[pred] must accept as many arguments as the number of input sequences @racket[seq].

@examples[
    #:eval eval-for-docs
	(index-where positive? (list -3 -2 -1 0 1 2 3))
	(index-where > (list 1 2 3 4) (list 2 3 1 4))
  ]
}

@defproc[(nth [pos natural-number/c]
              [seq sequence?])
         sequence?]{

 Similar to @racketlink[d:nth]{@racket[nth]} but also supports negative indexing to count backwards from the end of the sequence.

@examples[
    #:eval eval-for-docs
    (nth 3 (list 1 2 3 4 5))
    (nth 4 "The quick brown fox")
    (nth -1 (list "apple" "cherry" "banana"))
  ]
}

@defproc[(set-nth [pos natural-number/c]
                  [new-elt any/c]
                  [seq sequence?])
         sequence?]{

 Similar to @racketlink[d:set-nth]{@racket[set-nth]} but also supports negative indexing to count backwards from the end of the sequence.

@examples[
    #:eval eval-for-docs
    (set-nth 3 0 (list 1 2 3 4 5))
    (set-nth 4 "Q" "The quick brown fox")
    (set-nth -1 "durian" (list "apple" "cherry" "banana"))
  ]
}

@defproc[(remove-at [pos natural-number/c]
                    [seq sequence?])
         sequence?]{

 Remove the element at index @racket[pos] from @racket[seq].

@examples[
    #:eval eval-for-docs
    (remove-at 3 (list 1 2 3 4 5))
    (remove-at 3 "The quick brown fox")
    (remove-at 1 (list "apple" "cherry" "banana"))
  ]
}

@defproc[(truncate [seq sequence?]
                   [ref-seq sequence?])
         sequence?]{

 Truncate @racket[seq] so that its length does not exceed that of @racket[ref-seq]. Equivalent to @racket[(zip-with (arg 0))].

@examples[
    #:eval eval-for-docs
    (truncate "I wandered lonely as a cloud." "Max. tweet length.")
    (truncate "Nevermore." "Max. tweet length.")
    (->list (truncate (repeat "apple") "apple"))
    (->string (truncate (drop 2 (cycle "apple")) "apple"))
  ]
}

@subsection{Specific Element}

Refer to and reason in terms of specific elements contained in sequences.

@defproc[(find [pred procedure?]
               [seq sequence?]
               ...)
         any/c]{

 Find the first element in @racket[seq] that satisfies @racket[pred]. If no item satisfies the predicate, then the result is @racket[#f]. If more than one sequence is provided, @racket[pred] must accept as many arguments as there are sequences, and the result is a list of the first elements that satisfy it.

@examples[
    #:eval eval-for-docs
    (find number? (list "cherry" 'banana 10 30))
    (find positive? (list -1 -2 -1 2 3))
    (find (curry prefix? "ap") (list "banana" "apple" "apricot"))
    (find > (list -1 -2 -1 2 3) (list -1 3 1 0 1))
  ]
}

@defproc[(remove [#:key key procedure? #f]
                 [#:how-many how-many exact-nonnegative-integer? #f]
                 [elem any/c]
                 [seq sequence?])
         exact-nonnegative-integer?]{

 Remove occurrences of @racket[elem] from @racket[seq]. If @racket[how-many] is not specified, then all occurrences are removed. The @racket[key] argument, if provided, is passed through to the underlying generic equality relation, @racketlink[r:=]{@racket[=]} used to identify occurrences of @racket[elem].

@examples[
    #:eval eval-for-docs
    (remove 3 (list 1 2 3 4 5))
    (remove " " "The quick brown fox")
    (remove #:key string-upcase "cherry" (list "Apple" "CHERry" "BaNaNa"))
    (->list (remove #:key (curryr remainder 3) 1 #:how-many 2 (range 10)))
  ]
}

@subsection{Filtering}

Extract a subsequence.

@deftogether[(
@defproc[(filter [pred procedure?]
                 [seq sequence?])
         sequence?]
@defproc[(take-when [pred procedure?]
                    [seq sequence?])
         sequence?]
@defproc[(drop-when [pred procedure?]
                    [seq sequence?])
         sequence?]
)]{

 An alias for @racketlink[d:filter]{@racket[filter]}, @racket[take-when] selects all elements from @racket[seq] that satisfy @racket[pred], while @racket[drop-when] selects those elements that do not satisfy @racket[pred]. @racket[filter] and @racket[take-when] are identical to @racketlink[d:filter]{@racket[filter]} from @racket[data/collection], except that they include additional runtime annotations to support isomorphic behavior.

@examples[
    #:eval eval-for-docs
    (filter positive? #(1 -2 3))
    (take-when positive? (list 1 -4 -1 3))
    (drop-when positive? (list 1 -4 -1 3))
    (take-when (curry prefix? "ap") (list "banana" "apple" "apricot" "cherry"))
    (drop-when char-whitespace? "  the quick   \tbrown\nfox")
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
    (take-while positive? (list 1 2 -4 -12 3))
    (drop-while positive? (list 1 2 -4 -12 3))
    (take-while positive? (list -1 3 2 4 -12))
    (drop-while positive? (list -1 3 2 4 -12))
    (take-while (curry prefix? "ap") (list "apple" "banana" "apricot" "cherry"))
    (drop-while (curry prefix? "ap") (list "apple" "banana" "apricot" "cherry"))
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
    (take-until positive? (list -1 -2 3 2 -4))
    (drop-until positive? (list -1 -2 3 2 -4))
    (take-until positive? (list 1 3 2 -4))
    (drop-until positive? (list 1 3 2 -4))
    (take-until (curry prefix? "ap") (list "banana" "apple" "apricot" "cherry"))
    (drop-until (curry prefix? "ap") (list "banana" "apple" "apricot" "cherry"))
  ]
}

@defproc[(deduplicate [#:key key (-> any/c any/c) #f]
                      [seq sequence?])
         list?]{

 Remove duplicate occurrences of elements in @racket[seq], using the generic equivalence relation @racketlink[r:=]{@racket[=]} to determine equality.

@examples[
    #:eval eval-for-docs
    (deduplicate (list 1 2 3 "hi" 3 "bye" 1 2 "hi"))
    (deduplicate #:key string-upcase (list "hi" "hello" "HI" "HeLLo"))
  ]
}

@defproc[(trim-if [pred procedure?]
                  [seq sequence?]
                  [#:side side (one-of/c 'left 'right 'both) 'both]
                  [#:how-many how-many exact-nonnegative-integer? #f])
         sequence?]{

 Trim elements on one or both sides of @racket[seq] as long as they satisfy @racket[pred], terminating at the first element that does not.

@examples[
    #:eval eval-for-docs
    (trim-if negative? (list -1 0 1 2 3 -2 -1))
    (trim-if char-whitespace? "  \t the quick brown fox\n ")
  ]
}

@defproc[(trim [elem any/c]
               [seq sequence?]
               [#:side side (one-of/c 'left 'right 'both) 'both]
               [#:how-many how-many exact-nonnegative-integer? #f])
         sequence?]{

 Trim occurrences of @racket[elem] (under the equivalence check @racketlink[r:=]{@racket[=]}) on one or both sides of @racket[seq], terminating at the first element that is not @racketlink[r:=]{@racket[=]} to @racket[elem].

@examples[
    #:eval eval-for-docs
    (trim -1 (list -1 0 1 2 3 -2 -1))
    (trim " " "  \t the quick brown fox\n ")
  ]
}

@defproc[(trim-by [left exact-nonnegative-integer?]
                  [right exact-nonnegative-integer?]
                  [seq sequence?])
         sequence?]{

 Trim @racket[seq] on the left by @racket[left] elements, and on the right by @racket[right] elements.

@examples[
    #:eval eval-for-docs
    (trim-by 1 2 (list -1 0 1 2 3 -2 -1))
    (trim-by 4 5 "the quick brown fox\n")
  ]
}

@subsection{Infixes}

Refer to and reason in terms of contiguous subsequences, or "infixes."

@defproc[(cut-when [pred procedure?]
                   [seq sequence?])
         sequence?]{

 Cut a subsequence from @racket[seq] at each point where @racket[pred] is satisfied.

@examples[
    #:eval eval-for-docs
    (->list (cut-when (curry = #\space) "hello there old friend"))
	(->list (cut-when negative? (list -1 4 1 -3 2 -5 3 7)))
  ]
}

@defproc[(cut [elem any/c]
              [seq sequence?]
			  [#:key key procedure? #f])
         sequence?]{

 Similar to @racket[string-split] but generalized to work on any sequence, this cuts a subsequence from @racket[seq] at each point where @racket[elem] is encountered, excluding @racket[elem]. In some contexts this operation is called "tokenization."

 In the special case where the input sequence is a string, @racket[elem] may be either a @tech/reference{character} or a string representing a character. If the "trim" behavior of @racket[string-split] is desired, just @racket[trim] or @racket[trim-if] (e.g. using @racket[char-whitespace?]) the sequence prior to calling @racket[cut]. The @racket[key] argument, if provided, is passed through to the underlying generic equality relation, @racketlink[r:=]{@racket[=]}.

@examples[
    #:eval eval-for-docs
    (->list (cut " " "hello there old friend"))
	(->list (cut 1 (list -1 4 1 -3 2 -5 1 3 7)))
  ]
}

@defproc[(cut-at [pos exact-nonnegative-integer?]
                 [seq sequence?])
         (values sequence? sequence?)]{

 Cut @racket[seq] at the index @racket[pos], resulting in a pair of subsequences.

@examples[
    #:eval eval-for-docs
	(define-values (before after) (cut-at 11 "hello there old friend"))
	(list before after)
	(define-values (before after) (cut-at 3 (list -1 4 1 -3 2 -5 3 7)))
	(list before after)
  ]
}

@defproc[(cut-where [pred (-> any/c boolean?)]
                    [seq sequence?])
         (values sequence? sequence?)]{

 Cut @racket[seq] at the (first) point where @racket[pred] returns true, resulting in a pair of subsequences.

@examples[
    #:eval eval-for-docs
	(define-values (before after) (cut-where char-whitespace? "hello there old friend"))
	(list before after)
	(define-values (before after) (cut-where positive? (list -2 -1 0 1 2 3 4)))
	(list before after)
  ]
}

@defproc[(cut-by [n (-> any/c boolean?)]
                 [seq sequence?])
         (values sequence? sequence?)]{

 Cut @racket[seq] into subsequences of length @racket[n].

@examples[
    #:eval eval-for-docs
	(->list (cut-by 5 "hello there old friend"))
	(->list (cut-by 3 (list -2 4 1 -3 2 -5 3 7)))
  ]
}

@defproc[(cut-with [pred (-> any/c boolean?)]
                   [seq sequence?])
         (values sequence? sequence?)]{

 Similar to @racket[partition], use a predicate @racket[pred] to cut @racket[seq] into two subsequences, one containing those elements for which @racket[pred] holds, and the other containing those elements of @racket[seq] for which @racket[pred] fails.

@examples[
    #:eval eval-for-docs
    (define-values (yes no)
                   (cut-with (curry prefix? "ap")
                             (list "banana" "apple" "apricot" "cherry")))
    (list yes no)
    (define-values (yes no)
                   (cut-with positive?
                             (list -2 4 1 -3 2 -5 3 7)))
    (list yes no)
  ]
}

@defproc[(find-infix [#:key key (-> any/c any/c) #f]
                     [nfx sequence?]
                     [seq sequence?])
         any/c]{

 Finds the first occurrence of the subsequence @racket[nfx] in @racket[seq] and returns its index, if present, or @racket[#f] otherwise. The infixes are compared using the generic @racketlink[r:=]{@racket[=]} relation, and any provided @racket[key] for comparison is forwarded to it.

@examples[
    #:eval eval-for-docs
    (find-infix "fox" "the quick brown fox jumps over the lazy dog")
    (find-infix (range 3 5) (range 10))
    (find-infix "fish" "the quick brown fox jumps over the lazy dog")
  ]
}

@defproc[(replace-infix [#:key key (-> any/c any/c) #f]
                        [#:how-many how-many exact-nonnegative-integer? #f]
                        [nfx sequence?]
                        [new-nfx sequence?]
                        [seq sequence?])
         any/c]{

 Replaces the first @racket[how-many] occurrences of the subsequence @racket[nfx] in @racket[seq] with @racket[new-nfx]. The infixes are compared using the generic @racketlink[r:=]{@racket[=]} relation, and any provided @racket[key] for comparison is forwarded to it. If @racket[how-many] is not specified, all occurrences are replaced.

@examples[
    #:eval eval-for-docs
    (replace-infix "fox" "bear" "the quick brown fox jumps over the lazy dog")
    (->list (replace-infix (range 3 5) (range 13 21 2) (range 10)))
  ]
}

@subsection{Variations}

Derive sequences from an existing sequence.

@defproc[(suffixes [seq sequence?])
         sequence?]{

 A sequence of all suffixes or "tails" of @racket[seq].

@examples[
    #:eval eval-for-docs
    (->list (suffixes (list 1 2 3 4 5)))
	(->list (suffixes "echo"))
    (define (fibs)
      (stream-cons 1
        (stream-cons 1
          (apply zip-with + (take 2 (suffixes (fibs)))))))
    (->list (take 10 (fibs)))
  ]
}

@defproc[(prefixes [seq sequence?])
         sequence?]{

 A sequence of all prefixes of @racket[seq].

@examples[
    #:eval eval-for-docs
	(->list (prefixes "wild west"))
    (->list (take 5 (map ->list (prefixes (naturals)))))
  ]
}

@defproc[(infixes [len exact-positive-integer?] [seq sequence?])
         sequence?]{

 A sequence of all infixes of @racket[seq] of length @racket[len].

@examples[
    #:eval eval-for-docs
	(->list (infixes 4 "avocado"))
	(->list (take 5 (infixes 3 (naturals))))
  ]
}

@subsection{Predicates}

Assert or deny properties of sequences.

@defproc[(exists [pred (-> any/c boolean?)]
                 [seq sequence?]
                 ...)
         boolean?]{

 Similar to @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html#node_idx_206"]{exists} but generalized to all sequences rather than only lists, this checks if @emph{any} of the sequence values fulfill a provided predicate. @racket[pred] must accept a number of arguments equal to the number of provided sequences @racket[seq]. This is an alias for @racketlink[d:ormap]{ormap}.

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

 Similar to @hyperlink["https://docs.racket-lang.org/r6rs/r6rs-lib-std/r6rs-lib-Z-H-4.html#node_idx_204"]{for-all} but generalized to all sequences rather than only lists, this checks if @emph{all} of the sequence values fulfill a provided predicate. @racket[pred] must accept a number of arguments equal to the number of provided sequences @racket[seq]. This is an alias for @racketlink[d:andmap]{andmap}.

@examples[
    #:eval eval-for-docs
    (for-all positive? (list -1 3 0 2 5))
    (for-all positive? (list 1 3 2 2 5))
    (for-all < (list 1 2 3 4 5) (list 2 3 4 5 6))
  ]
}

@deftogether[(
@defproc[(prefix? [nfx sequence?]
                  [seq sequence?]
                  ...)
         boolean?]
@defproc[(starts-with? [nfx sequence?]
                       [seq sequence?]
                       ...)
         boolean?]
@defproc[(suffix? [nfx sequence?]
                  [seq sequence?]
                  ...)
         boolean?]
@defproc[(ends-with? [nfx sequence?]
                     [seq sequence?]
                     ...)
         boolean?]
@defproc[(infix? [nfx sequence?]
                 [seq sequence?]
                 ...)
         boolean?]
@defproc[(contains? [nfx sequence?]
                    [seq sequence?]
                    ...)
         boolean?]
)]{

 @racket[prefix?] / @racket[starts-with?] checks if the sequence @racket[seq] contains @racket[nfx] at its head.
 @racket[suffix?] / @racket[ends-with?] checks if the sequence @racket[seq] contains @racket[nfx] at its tail end.
 @racket[infix?] / @racket[contains?] checks if the sequence @racket[seq] contains @racket[nfx].

@examples[
    #:eval eval-for-docs
    (prefix? (list 1 3) (list 1 3 0 2 5))
    (prefix? "ap" "apricot")
    (suffix? (list 2 5) (list 1 3 0 2 5))
    (suffix? "cot" "apricot")
    (infix? (list 3 0) (list 1 3 0 2 5))
    (infix? "rico" "apricot")
  ]
}

@subsection{Defining}

Construct new sequences from primitive elements and other sequences. Not to be confused with @seclink["Composing" #:doc '(lib "seq/scribblings/seq.scrbl")]{composing} sequences.

@defproc[(multiples [elem any/c] [n natural-number/c 0])
         sequence?]{

 A sequence of all multiples of @racket[elem] starting at the @racket[n]'th one.

@examples[
    #:eval eval-for-docs
    (->list (take 10 (multiples 3)))
    (->list (take 10 (multiples 3 1)))
    (->list (take 10 (map add1 (multiples 3))))
  ]
}

@defproc[(powers [elem any/c] [op (one-of/c ~ + *) ~])
         sequence?]{

 A sequence of all @racketlink[r:power]{powers} of @racket[elem] under the operation @racket[op].

@examples[
    #:eval eval-for-docs
    (->list (take 10 (powers 3)))
    (->list (take 10 (powers 3 *)))
    (->list (take 4 (powers "abc")))
    (->list (take 4 (powers '(1 2 3))))
    (->list (take 10 (onto (powers add1) 0)))
    (define (double x) (* 2 x))
    (->list (take 10 (onto (powers double) 2)))
  ]
}

@defproc[(iterate [f procedure?] [elem any/c])
         sequence?]{

 A sequence obtained by repeated application of @racket[f], starting with the seed value @racket[elem].

@examples[
    #:eval eval-for-docs
    (->list (take 10 (iterate add1 3)))
    (->list (take 10 (iterate (power add1 2) 3)))
    (->list (take 5 (iterate sqr 2)))
    (define (double x) (* 2 x))
    (->list (take 10 (iterate double 2)))
  ]
}

@;{intersperse has type T,Seq[T] -> Seq[T], while join-with has type T,Seq[T] -> T. These involve composition, in the former case of the members of seq lifted to Seq, and in the latter case, of the members of seq themselves (indirectly, via an endofunctor), but they are not compositions of provided sequences as the preceding interfaces are. May warrant a separate category.}

@deftogether[(
@defproc[(intersperse [elem any/c]
                      [seq sequence?])
         sequence?]
@defproc[(add-between [elem any/c]
                      [seq sequence?])
         sequence?]
)]{

 Similar to @racketlink[b:add-between]{@racket[add-between]}, create a new sequence by adding @racket[elem] between the elements of @racket[seq].

@examples[
    #:eval eval-for-docs
    (intersperse 'and '(x y z))
    (intersperse 'and '(x))
    (intersperse "," '("a" "b" "c" "d"))
  ]
}

@defproc[(wrap-each [before any/c]
                    [after any/c]
                    [seq sequence?])
         sequence?]{

 Create a new sequence by wrapping each element of @racket[seq] with @racket[before] and @racket[after].

@examples[
    #:eval eval-for-docs
    (wrap-each '< '> '(x y z))
    (wrap-each '< '> '(x))
    (join-with " " (wrap-each "fresh" "and" '("apples" "bananas" "cherries")))
    ((join (wrap-each ->string ->number (list add1 sqr))) "3")
  ]
}

@defproc[(join-with [elem any/c]
                    [seq sequence?])
         any/c]{

 Similar to @racketlink[r:join]{@racket[join]}, but @racketlink[intersperse]{intersperses} @racket[elem] between the elements of @racket[seq] prior to joining them together. The result is of the same type as @racket[elem] and the members of @racket[seq].

@examples[
    #:eval eval-for-docs
    (join-with " " (list "hello" "there" "old" "friend"))
    (display (join-with "\n" (list "Item 1" "Item 2" "Item 3")))
    (join-with '(0 0) (stream '(1 2 3) '(4 5 6) '(7 8 9)))
    (join-with 1 (list 1 2 3 4))
    ((join-with (λ (n)
                   (displayln n)
                   n)
                (list number->string sub1 sqr add1 sqr))
     3)
  ]
}

@defproc[(weave [before any/c]
                [after any/c]
                [seq sequence?])
         sequence?]{

 Similar to @racketlink[r:join]{@racket[join]}, but @racketlink[wrap-each]{wraps} each element of @racket[seq] with @racket[to] and @racket[from] prior to joining them together. The result is of the same type as @racket[to], @racket[from], and the members of @racket[seq].

@examples[
    #:eval eval-for-docs
    ((weave ->string
            ->number
            (list add1
                  ((^ 2) add1)
                  ((^ 3) add1)))
     "7")
    (weave "fresh " " and " '("apples" "bananas" "cherries"))
    ((weave ->string ->number (list add1 sqr)) "3")
  ]
}

@defproc*[([(range [end number?]) stream?]
           [(range [start number?]
                   [end number?]
                   [step number? 1]) stream?])]{
 Identical to @racketlink[b:range]{@racket[range]}, except that it includes additional runtime annotations to support isomorphic behavior.
}

@defproc[(map [proc procedure?] [seq sequence?] ...+)
         sequence?]{

 Identical to @racketlink[d:map]{@racket[map]} from @racket[data/collection], except that it includes additional runtime annotations to support isomorphic behavior.

@examples[
    #:eval eval-for-docs
    (map sqr (list 1 2 3))
    (map sqr #(1 2 3))
  ]
}

@subsection{Composing}

Compose new sequences from given sequences. Not to be confused with @seclink["Defining" #:doc '(lib "seq/scribblings/seq.scrbl")]{defining} sequences.

@deftogether[(
@defproc[(zip [seq sequence?]
              ...)
         (sequenceof list?)]
@defproc[(zip-with [op procedure?]
                   [seq sequence?]
                   ...)
         (sequenceof any/c)]
@defproc[(unzip [seq sequence?])
         (sequenceof list?)]
@defproc[(unzip-with [op procedure?]
                     [seq sequence?])
         (sequenceof any/c)]
)]{

 @racket[zip-with] merges the input sequences using the provided operation @racket[op]. Equivalent to Haskell's @hyperlink["http://zvon.org/other/haskell/Outputprelude/zipWith_f.html"]{zipWith}.

 @racket[zip] is equivalent to @racket[zip-with list].

 @racket[zip] is its own inverse, so applying it twice is essentially equivalent to doing nothing. Still, @racket[unzip-with] and @racket[unzip] are provided with a slightly different signature -- accepting a single sequence rather than an arbitrary number of sequences, making it convenient to apply to a result already derived by using @racket[zip]. @racket[unzip] is equivalent to @racket[(curry apply zip)].

@examples[
    #:eval eval-for-docs
    (zip (list 'a 'b 'c) (list 1 2 3 4 5))
    (zip-with + (list 1 2 3) (list 3 2 1))
    (->list (zip-with expt (repeat 5) (range 10)))
    (->list (zip-with (lambda (x y)
                        (+ (* 2 x)
                           y))
                      (range 1 5)
                      (range 5 9)))
    (unzip (zip (list 'a 'b 'c) (list 1 2 3)))
  ]
}

@defproc[(interleave [seq sequence?]
                     ...)
         sequence?]{

 Lazily form a sequence by taking elements one at a time, in turn, from each of the input sequences, stopping at the first input sequence that runs out of elements.

@examples[
    #:eval eval-for-docs
    (interleave (list 1 2 3) (list 4 5 6) (list 7 8 9))
    (interleave (list 'a 'b 'c) (list 1 2))
    (->list (take 10 (interleave (naturals 1) (cycle (list 'A 'B)))))
    (->list (interleave (naturals 1) (list 'P 'Q 'R 'S 'T) (cycle (list 'a 'b))))
  ]
}

@defproc[(choose [pred procedure?]
                 [seq sequence?]
                 ...)
         sequence?]{

 Lazily choose a single item from each of the input sequences -- the first one that fulfills the choice predicate @racket[pred]. The result is a sequence containing as many values as the number of input sequences. If no item in a particular sequence fulfills the choice predicate, then the corresponding element in the resulting sequence is @racket[#f].

@examples[
    #:eval eval-for-docs
    (choose number? (list 10 "left shoe" 30) (list "right shoe" 15 15) (list "sock" -55 7))
    (choose positive? (list -1 -2 1 2) (list -5 3 -2) (list 5 2 -1))
    (choose (curry prefix? "ap") (list "banana" "apple" "apricot") (list "dog" "cat" "ape"))
  ]
}

@subsection{Permuting}

Rearrange the elements of sequences.

@deftogether[(
@defproc[(rotate-left [n exact-nonnegative-integer?]
                      [seq sequence?])
         sequence?]
@defproc[(rotate-right [n exact-nonnegative-integer?]
                       [seq sequence?])
         sequence?]
@defproc[(rotate [seq sequence?])
         sequence?]
@defproc[(rotations [seq sequence?])
         (sequenceof sequence?)]
)]{
 Derive a new sequence by shifting the elements of @racket[seq] to the left or to the right, wrapping around the tail of the list. @racket[rotate-left] eagerly processes @racket[n] elements of @racket[seq] and otherwise lazily evaluates the result, while @racket[rotate-right] processes the entire list and therefore is @emph{not} lazy.

 @racket[rotate] is equivalent to @racket[(curry rotate-left 1)].

 @racket[rotations] yields all distinct rotations of @racket[seq].

@examples[
    #:eval eval-for-docs
    (->list (rotate-left 1 (range 1 8)))
    (->list (rotate-right 1 (range 1 8)))
    (->list (rotate-left 3 (range 1 8)))
    (->list (rotate-right 3 (range 1 8)))
    (rotate-left 2 "avocado")
    (rotate-right 2 "avocado")
    (rotate "avocado")
    ((power rotate 3) "avocado")
	(->list (rotations '(1 2 3)))
	(->list (rotations "cherry"))
	(->list (truncate (iterate rotate "cherry") "cherry"))
  ]
}

@defproc[(reverse [seq sequence?])
         sequence?]{

 Identical to @racketlink[d:reverse]{@racket[reverse]} from @racket[data/collection], except that it includes additional runtime annotations to support isomorphic behavior.

@examples[
    #:eval eval-for-docs
    (reverse "apple")
    (reverse #(1 2 3))
  ]
}
