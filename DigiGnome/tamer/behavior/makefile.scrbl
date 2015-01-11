#lang scribble/manual

@(require scribble/lp-include)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))

@title[#:style (list 'non-toc) #:version (format "~a[~a]" (version) (info-ref 'version))]{Tamer's Handbook for makefile.rkt}

@margin-note{It's reasonable to suppose that you have already know the project conventions.
             But if you reach here through a strange way, you still have the
             @hyperlink["https://github.com/digital-world/DigiGnome"]{README} page.}
This is a special handbook that defines the behavior of
@hyperlink[(collection-file-path "makefile.rkt" (getenv "digimon-gnome"))]{the building system} itself.

@;{
@filebox[@italic{Behavioral Specification}]{
                                            @bold{Title: The story should have a clear, explicit title.}
                                             
                                             @bold{Narrative}
                                             
                                             A short, introductory section that specifies
                                             @(itemlist #:style 'compact
                                                        @item{who is the driver or primary stakeholder of the story}
                                                        @item{which effect the stakeholder wants the story to have}
                                                        @item{what business value the stakeholder will derive from this effect})
                                             
                                             @bold{Acceptance criteria or scenarios}
                                             
                                             a description of each specific case of the narrative. Such a scenario has the following structure:
                                             @(itemlist #:style 'compact
                                                        @item{It starts by specifying the initial condition that is assumed to be true at the beginning of the scenario.
                                                              This may consist of a single clause, or several.}
                                                        @item{It then states which event triggers the start of the scenario.}
                                                        @item{Finally, it states the expected outcome, in one or more clauses.})}
}
