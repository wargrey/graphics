#lang scribble/manual

@(require "tamer.rkt")
@(require (submod "tamer.rkt" makefile))

@(require scribble/core)
@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))

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

@title[#:style (list 'non-toc) #:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Tamer@literal{'}s Handbook}}

@margin-note{It@literal{'}s reasonable to suppose that you have already know the project conventions.
             But if you reach here through a strange way, you still have the
             @hyperlink["https://github.com/digital-world/DigiGnome"]{README} page.}
This handbook is the sample of applying
@hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Behavior Driven Development} with
@hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{Literate Programming} via testing
@(for/first ([mkfile (in-list (list (collection-file-path "makefile.rkt" (getenv "digimon-gnome"))
                                    (build-path (getenv "digimon-world") "makefile.rkt")))]
             #:when (file-exists? mkfile))
   @hyperlink[mkfile]{makefile.rkt}) itself
that the entire project should follow since the @italic{Behavior Driven Development} plays an important role in my personal software development process.

@italic{Translating is the most complex human action in the Universe.}

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

The suggested @italic{Behavioral Specification}s are always written as the @italic{ubiquitous language}s, trying to avoid the communication breakdowns
between Developers and Bussiness Stakeholders. While I@literal{'}m an indenpendent developer and communicating costs almost nothing.
So I make decision at my risk to write @italic{behavioral specification} and test suites apart from the production code with the @italic{Literate Programming} skill
so that the @italic{handbook} can also plays a role of the test report as well as the design documents.

@italic{@bold{Principal} This sample should be (rather than must be) followed due to the complexity of the real world problems.
         In fact it@literal{'}s all right to forget it after reading.}

@lp-include{makefile.rkt}
