#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/string)
(require racket/math)

(require "paint.rkt")

(require "../convert.rkt")
(require "../source.rkt")
(require "../font.rkt")

(require "../unsafe/dc/text.rkt")
(require "../unsafe/font.rkt")

(require "../../font.rkt")
(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:string geo
  ([body : String])
  #:type-name Geo:String
  #:transparent)

(struct geo:art-text geo:string
  ([lines : (Listof Symbol)])
  #:type-name Geo:Art-Text
  #:transparent)

(struct geo:text geo:string
  ([lines : (Listof Symbol)]
   [aline : Maybe-Stroke-Paint]
   [dline : Maybe-Stroke-Paint]
   [cline : Maybe-Stroke-Paint]
   [mline : Maybe-Stroke-Paint]
   [bline : Maybe-Stroke-Paint])
  #:type-name Geo:Text
  #:transparent)

(struct geo:para geo:string
  ([lines : (Listof Symbol)]
   [mwidth : (Option Flonum)]
   [mheight : (U Flonum Nonpositive-Integer)]
   [ident : Flonum]
   [space : Flonum]
   [wmode : Integer]
   [emode : Integer])
  #:type-name Geo:Paragraph
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-art-text : (->* (Any)
                            ((Option Font) #:id (Option Symbol) #:lines (Listof Symbol)
                                           #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:background Maybe-Fill-Paint)
                            Geo:Art-Text)
  (lambda [#:id [id #false] #:stroke [outline (void)] #:fill [fill (void)] #:background [bgsource (void)] #:lines [lines null] text [font #false]]
    (create-geometry-object geo:art-text
                            #:surface (geo-art-text-surface font outline fill bgsource)
                            #:extent (geo-art-text-extent font)
                            #:id id
                            (~a text) lines)))

(define geo-text : (->* (Any)
                        ((Option Font) #:id (Option Symbol) #:color Option-Fill-Paint #:background Maybe-Fill-Paint #:lines (Listof Symbol)
                                       #:baseline Maybe-Stroke-Paint #:capline Maybe-Stroke-Paint #:meanline Maybe-Stroke-Paint
                                       #:ascent Maybe-Stroke-Paint #:descent Maybe-Stroke-Paint)
                        Geo:Text)
  (lambda [#:id [id #false] #:color [fgsource #false] #:background [bgsource (void)] #:lines [lines null]
           #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false] #:baseline [blsource #false]
           text [font #false] ]
    (create-geometry-object geo:text
                            #:surface (geo-text-surface font fgsource bgsource)
                            #:extent (geo-text-extent font)
                            #:id id
                            (~a text) lines alsource dlsource clsource mlsource blsource)))

(define geo-paragraph : (->* ((U String (Listof String)))
                             ((Option Font) #:id (Option Symbol) #:color Option-Fill-Paint #:background Maybe-Fill-Paint #:lines (Listof Symbol)
                                            #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                            #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode)
                             Geo:Paragraph)
  (lambda [texts [font #false] #:id [id #false] #:color [fgsource #false] #:background [bgsource (void)] #:lines [lines null]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]]
    (define-values (smart-height smart-emode)
      (cond [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (create-geometry-object geo:para
                            #:surface (geo-paragraph-surface font fgsource bgsource)
                            #:id id
                            (if (list? texts) (string-join texts "\n") texts) lines
                            (if (or (infinite? max-width) (nan? max-width)) #false (real->double-flonum max-width)) smart-height
                            (real->double-flonum indent) (real->double-flonum spacing)
                            (paragraph-wrap-mode->integer wrap-mode raise-argument-error)
                            (paragraph-ellipsize-mode->integer smart-emode raise-argument-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-text-extent : (-> (Option Font) Geo-Calculate-Extent)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:text?])
        (define-values (W H) (text-size (geo:string-body self) (or alt-font (default-font))))
        (values W H #false)))))

(define geo-art-text-extent : (-> (Option Font) Geo-Calculate-Extent)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:art-text?])
        (define-values (W H) (text-size (geo:string-body self) (or alt-font (default-art-font))))
        (values W H #false)))))

(define geo-text-surface : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self]
      (with-asserts ([self geo:text?])
        (dc_text create-abstract-surface
                 (geo:string-body self) (geo-select-font alt-font default-font) (geo:text-lines self)
                 (geo-select-font-source alt-fg) (geo-select-background-source alt-bg)
                 (stroke-paint->source* (geo:text-aline self)) (stroke-paint->source* (geo:text-cline self))
                 (stroke-paint->source* (geo:text-mline self))
                 (stroke-paint->source* (geo:text-bline self)) (stroke-paint->source* (geo:text-dline self))
                 (default-geometry-density))))))

(define geo-art-text-surface : (-> (Option Font) Maybe-Stroke-Paint Maybe-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-outl alt-fill alt-bg]
    (λ [self]
      (with-asserts ([self geo:art-text?])
        (dc_art_text create-abstract-surface
                     (geo:string-body self) (geo-select-font alt-font default-art-font) (geo:art-text-lines self)
                     (geo-select-stroke-paint alt-outl) (geo-select-fill-source alt-fill) (geo-select-background-source alt-bg)
                     (default-geometry-density))))))

(define geo-paragraph-surface : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self]
      (with-asserts ([self geo:para?])
        (dc_paragraph create-abstract-surface
                      (geo:string-body self) (geo-select-font alt-font default-font) (geo:para-lines self)
                      (geo:para-mwidth self) (geo:para-mheight self)
                      (geo:para-ident self) (geo:para-space self) (geo:para-wmode self) (geo:para-emode self)
                      (geo-select-font-source alt-fg) (geo-select-background-source alt-bg)
                      (default-geometry-density))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-font : (-> (Option Font) (-> Font) Font-Description)
  (lambda [alt-font fallback-font]
    (font-description (or alt-font (fallback-font)))))
