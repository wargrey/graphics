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
(require "../unsafe/dc/toytext.rkt")
(require "../unsafe/font.rkt")

(require "../../font.rkt")
(require "../../paint.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:toy-text geo
  ([content : String])
  #:type-name Geo:Toy-Text
  #:transparent)

(struct geo:text geo
  ([content : String]
   [lines : (Listof Symbol)]
   [aline : Maybe-Stroke-Paint]
   [dline : Maybe-Stroke-Paint]
   [cline : Maybe-Stroke-Paint]
   [mline : Maybe-Stroke-Paint]
   [bline : Maybe-Stroke-Paint])
  #:type-name Geo:Text
  #:transparent)

(struct geo:para geo
  ([content : String]
   [lines : (Listof Symbol)]
   [mwidth : (Option Flonum)]
   [mheight : (U Flonum Nonpositive-Integer)]
   [ident : Flonum]
   [space : Flonum]
   [wmode : Integer]
   [emode : Integer])
  #:type-name Geo:Paragraph
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-toy-text : (->* (Any)
                            ((Option Font) #:id (Option Symbol) #:stroke Maybe-Stroke-Paint #:fill Maybe-Fill-Paint #:background Maybe-Fill-Paint)
                            Geo:Toy-Text)
  (lambda [#:id [id #false] #:stroke [outline (void)] #:fill [fill (void)] #:background [bgsource (void)] text [font #false]]
    (create-geometry-object geo:toy-text
                            #:with [(geo-toy-text-surface-make font outline fill bgsource) (geo-toy-text-calculate-bbox-make font)] #:id id
                            (~a text))))

(define geo-text : (->* (Any)
                        ((Option Font) #:id (Option Symbol) #:color Option-Fill-Paint #:background Maybe-Fill-Paint #:lines (Listof Symbol)
                                       #:baseline Maybe-Stroke-Paint #:capline Maybe-Stroke-Paint #:meanline Maybe-Stroke-Paint
                                       #:ascent Maybe-Stroke-Paint #:descent Maybe-Stroke-Paint)
                        Geo:Text)
  (lambda [#:id [id #false] #:color [fgsource #false] #:background [bgsource (void)] #:lines [lines null]
           #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false] #:baseline [blsource #false]
           text [font #false] ]
    (create-geometry-object geo:text
                            #:with [(geo-text-surface-make font fgsource bgsource) (geo-text-calculate-bbox-make font)] #:id id
                            (~a text) lines alsource dlsource clsource mlsource blsource)))

(define geo-paragraph : (->* ((U String (Listof String)))
                             (Font #:id (Option Symbol) #:color Option-Fill-Paint #:background Maybe-Fill-Paint #:lines (Listof Symbol)
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
                            #:with [(geo-paragraph-surface-make font fgsource bgsource)] #:id id
                            (if (list? texts) (string-join texts "\n") texts) lines
                            (if (or (infinite? max-width) (nan? max-width)) #false (real->double-flonum max-width)) smart-height
                            (real->double-flonum indent) (real->double-flonum spacing)
                            (paragraph-wrap-mode->integer wrap-mode raise-argument-error)
                            (paragraph-ellipsize-mode->integer smart-emode raise-argument-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-text-calculate-bbox-make : (-> (Option Font) Geo-Calculate-BBox)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:text?])
        (define-values (W H) (text-size (geo:text-content self) (or alt-font (default-font))))
        (values 0.0 0.0 W H)))))

(define geo-toy-text-calculate-bbox-make : (-> (Option Font) Geo-Calculate-BBox)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:toy-text?])
        (define-values (W H) (dc_toy_text_size (geo:toy-text-content self) (geo-select-font alt-font default-toy-font) (current-stroke-source)))
        (values 0.0 0.0 W H)))))

(define geo-text-surface-make : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self]
      (with-asserts ([self geo:text?])
        (dc_text create-abstract-surface
                 (geo:text-content self) (geo-select-font alt-font default-font) (geo:text-lines self)
                 (geo-select-foreground alt-fg) (geo-select-background alt-bg)
                 (stroke-paint->source* (geo:text-aline self)) (stroke-paint->source* (geo:text-cline self))
                 (stroke-paint->source* (geo:text-mline self))
                 (stroke-paint->source* (geo:text-bline self)) (stroke-paint->source* (geo:text-dline self))
                 (default-geometry-density))))))

(define geo-toy-text-surface-make : (-> (Option Font) Maybe-Stroke-Paint Maybe-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-outl alt-fill alt-bg]
    (λ [self]
      (with-asserts ([self geo:toy-text?])
        (dc_toy_text create-abstract-surface
                     (geo:toy-text-content self) (geo-select-font alt-font default-toy-font)
                     (geo-select-stroke-paint alt-outl) (geo-select-fill alt-fill) (geo-select-background alt-bg)
                     (default-geometry-density))))))

(define geo-paragraph-surface-make : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Create)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self]
      (with-asserts ([self geo:para?])
        (dc_paragraph create-abstract-surface
                      (geo:para-content self) (geo-select-font alt-font default-font) (geo:para-lines self)
                      (geo:para-mwidth self) (geo:para-mheight self)
                      (geo:para-ident self) (geo:para-space self) (geo:para-wmode self) (geo:para-emode self)
                      (geo-select-foreground alt-fg) (geo-select-background alt-bg)
                      (default-geometry-density))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-font : (-> (Option Font) (-> Font) Font-Description)
  (lambda [alt-font fallback-font]
    (font-description (or alt-font (fallback-font)))))
