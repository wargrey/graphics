#lang scribble/manual

@(require scribble/core)
@(require scribble/lp-include)

@(require setup/getinfo)

@require{tamer.rkt}

@(define info-ref (get-info/full (digimon-zone)))

@title[#:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Tamer@literal{'}s Handbook}}

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
model software system in @hyperlink["http://en.wikipedia.org/wiki/Formal_methods"]{@italic{Formal Methods}} and
document it with the @hyperlink["http://en.wikipedia.org/wiki/Literate_programming"]{@italic{Literate Programming}} approach.
After all this @italic{handbook} plays the role of
the @hyperlink["http://en.wikipedia.org/wiki/Behavior-driven_development"]{Test Report} along with
the @hyperlink["http://en.wikipedia.org/wiki/Design_by_contract"]{Design Documentation}
so that the production code could keep elegant.

@tamer-smart-summary[]
@table-of-contents[]

@lp-include{makefile.rkt}
