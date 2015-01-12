#lang scribble/manual

@(require racket)

@(require scribble/core)

@(require setup/getinfo)

@(define info-ref (get-info/full (getenv "digimon-zone")))

@(define part->blocks
   {lambda [psection header-level]
     (define blocks (foldl {lambda [sp bs] (append bs sp)}
                           (cons (para #:style (make-style "boxed" null)
                                       (larger ((cond [(zero? header-level) larger]
                                                      [else (apply compose1 (make-list (sub1 header-level) smaller))])
                                        (elem #:style (make-style (format "h~a" (add1 header-level)) null)
                                              (part-title-content psection)))))
                                 (part-blocks psection))
                           (map {lambda [sub] (part->blocks sub (add1 header-level))}
                                (part-parts psection))))
     (cond [(> header-level 0) blocks]
           [else (let ([dver (filter document-version? (style-properties (part-style psection)))])
                   (cond [(or (null? dver) (zero? (string-length (document-version-text (car dver))))) blocks]
                         [else (cons (elem #:style (make-style "tocsubtitle" null)
                                           (format "v.~a" (document-version-text (car dver))))
                                     blocks)]))])})

@title[#:version (format "~a[~a]" (version) (info-ref 'version))]{@bold{Making the @italic{Tamer@literal{'}s Handbook} as a story}}

@nested[#:style 'code-inset]{@racketoutput{@bold{Story:} Make a @italic{Tamer@literal{'}s Handbook} to Show the Sample}
                             
                              @racketoutput{@bold{In order to} standardize my development process and to make life joyful}@linebreak[]
                              @racketoutput{@bold{As an} indenpendent developer}@linebreak[]
                              @racketoutput{@bold{I want to} write the @italic{handbook} with @italic{Literate Programming} skill}
                              
                              @racketoutput{@bold{Scenario 1:} The project should provide @filepath{tamer/behavior/makefile.scrbl}}@linebreak[]
                              @racketoutput{@bold{Given} the project have been launched and ready for developing}@linebreak[]
                              @racketoutput{@bold{And} I currently have not found any appropriate specification templates}@linebreak[]
                              @racketoutput{@bold{When} I do some researching and architect a template}@linebreak[]
                              @racketoutput{@bold{Then} I should see the @filepath{makefile.scrbl} in @filepath{tamer/behavior}}
                              
                              @racketoutput{@bold{Scenario 2:} The project should provide @filepath{tamer/behavior/handbook.scrbl}}@linebreak[]
                              @racketoutput{@bold{Given} the building script checks @filepath{handbook.scrbl} to generate reports}@linebreak[]
                              @racketoutput{@bold{And} the behavior of the project is all about the building process itself}@linebreak[]
                              @racketoutput{@bold{And} the build process should be checked manually}@linebreak[]
                              @racketoutput{@bold{When} I play a trick on the building script to follow its rule}@linebreak[]
                              @racketoutput{@bold{Then} I should see the @filepath{handbook.scrbl} in @filepath{tamer/behavior}}
                              
                              @racketerror{Hmm@|._|@|._|@|._| the @italic{Gherkin language}, sounds abnormal and verbose.}@linebreak[]
                              @racketerror{Some test programers must get headache and stomachache.}}

@nested[#:style (make-style "boxed" null)]{@(let* ([makefile.scrbl (path->string (build-path (path-only (syntax-source #'handbook)) "makefile.scrbl"))]
                                                   [makefile-name (path->string (find-relative-path (getenv "digimon-zone") makefile.scrbl))]
                                                   [makedoc (part->blocks (dynamic-require `(file ,makefile.scrbl) 'doc) 0)])
                                              @filebox[@hyperlink[@|makefile.scrbl|]{@italic{@|makefile-name|}}]{@|makedoc|})}