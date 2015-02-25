#lang scribble/manual

@(require "tamer.rkt")

@(require scribble/core)
@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (digimon-zone)))

@(define part->blocks
   {lambda [psection [header-level 0]]
     (define blocks (foldl {λ [sp bs] (append bs sp)}
                           (cons (para #:style (make-style "boxed" null)
                                       (larger ((cond [(zero? header-level) larger]
                                                      [else (apply compose1 (make-list (sub1 header-level) smaller))])
                                        (elem #:style (make-style (format "h~a" (add1 header-level)) null)
                                              (part-title-content psection)))))
                                 (part-blocks psection))
                           (map {λ [sub] (part->blocks sub (add1 header-level))}
                                (part-parts psection))))
     (cond [(> header-level 0) blocks]
           [else (let ([dver (filter document-version? (style-properties (part-style psection)))])
                   (cond [(or (null? dver) (zero? (string-length (document-version-text (car dver))))) blocks]
                         [else (cons (elem #:style (make-style "tocsubtitle" null)
                                           (format "v.~a" (document-version-text (car dver))))
                                     blocks)]))])})

@title[#:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Tamer@literal{'}s Handbook}}

@margin-note{This handbook shows my @italic{Programming Methodology} via testing
                                    @(for/first ([mkfile (in-list (list (collection-file-path "makefile.rkt" (digimon-gnome))
                                                                        (build-path (digimon-world) "makefile.rkt")))]
                                                 #:when (file-exists? mkfile))
                                       @hyperlink[mkfile]{makefile.rkt}) itself (implies the
                                                                        @hyperlink[@(build-path (digimon-digitama) "runtime.rkt")]{minimal common code base})
                                                                        that the entire project should follow.
                                                                        
                                                                        @italic{@bold{Principal} This sample should be (rather than must be)
                                                                                 followed due to the complexity of the real world problems.
                                                                                 In fact it@literal{'}s all right to forget it after reading.}}

@italic{Translating is the most complex human activity in the Universe.}

@nested[#:style 'code-inset]{@racketoutput{@bold{Story:} Make a @italic{Tamer@literal{'}s Handbook} to Show the Sample}
                             
                              @racketoutput{@bold{In order to} standardize my development process and to make life joyful}@linebreak[]
                              @racketoutput{@bold{As an} indenpendent developer}@linebreak[]
                              @racketoutput{@bold{I want to} write the @italic{handbook} with @italic{Literate Programming} skill}
                              
                              @racketoutput{@bold{Scenario 1:} The project should provide @filepath{tamer/handbook.scrbl}}@linebreak[]
                              @racketoutput{@bold{Given} the project have been launched and ready for developing}@linebreak[]
                              @racketoutput{@bold{And} I currently have not found any appropriate specification templates}@linebreak[]
                              @racketoutput{@bold{When} I do some researching and architect a template}@linebreak[]
                              @racketoutput{@bold{Then} I should see the @filepath{handbook.scrbl} in @filepath{tamer}}
                              
                              @racketerror{Hmm@|._|@|._|@|._| the @italic{Gherkin language}, semiformal and elegant, or maybe stupid.@linebreak[]
                                              Nevertheless, it@literal{'}s not my cup of tea@|._|@|._|@|._|}}

Since I@literal{'}m an indenpendent developer and communicating costs almost nothing, I make decision at my risk to
model software system in @hyperlink["http://en.wikipedia.org/wiki/Formal_methods"]{Formal Methods} and
document it with the @hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming} approach.
After all this @italic{handbook} plays the role of
the @hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Test Report} along with
the @hyperlink["http://en.wikipedia.org/wiki/Design_by_contract"]{Design Documentation}
so that the production code could keep elegant.

@tamer-smart-summary[]
@table-of-contents[]

@lp-include{makefile.rkt}
