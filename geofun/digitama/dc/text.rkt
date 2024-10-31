#lang typed/racket/base

(provide (all-defined-out))

(require racket/format)
(require racket/string)
(require racket/math)

(require "../paint.rkt")
(require "../convert.rkt")
(require "../source.rkt")
(require "../font.rkt")

(require "../unsafe/dc/text-layout.rkt")
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
(define geo-art-text
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (void)] #:fill [fill : Maybe-Fill-Paint (void)] #:background [bgsource : Maybe-Fill-Paint (void)]
           #:id [id : (Option Symbol) #false] #:lines [lines : (Listof Symbol) null]
           [text : Any] [font : (Option Font) #false]] : Geo:Art-Text
    (create-geometry-object geo:art-text
                            #:with [id (geo-draw-art-text font outline fill bgsource)
                                       (geo-art-text-extent font)
                                       (geo-shape-outline outline)]
                            (~a text) lines)))

(define geo-text
  (lambda [#:color [fgsource : Option-Fill-Paint #false] #:background [bgsource : Maybe-Fill-Paint (void)]
           #:ascent [alsource : Maybe-Stroke-Paint #false] #:descent [dlsource : Maybe-Stroke-Paint #false] #:capline [clsource : Maybe-Stroke-Paint #false]
           #:meanline [mlsource : Maybe-Stroke-Paint #false] #:baseline [blsource : Maybe-Stroke-Paint #false]
           #:id [id : (Option Symbol) #false] #:lines [lines : (Listof Symbol) null]
           [text : Any] [font : (Option Font) #false]] : Geo:Text
    (create-geometry-object geo:text
                            #:with [id (geo-draw-text font fgsource bgsource)
                                       (geo-text-extent font)
                                       geo-zero-pads]
                            (~a text) lines alsource dlsource clsource mlsource blsource)))

(define geo-paragraph
  (lambda [#:color [fgsource : Option-Fill-Paint #false] #:background [bgsource : Maybe-Fill-Paint (void)]
           #:max-width [max-width : Real +inf.0] #:max-height [max-height : Real +inf.0] #:indent [indent : Real 0.0] #:spacing [spacing : Real 0.0]
           #:wrap-mode [wrap-mode : Paragraph-Wrap-Mode 'word-char] #:ellipsize-mode [ellipsize-mode : Paragraph-Ellipsize-Mode 'end]
           #:id [id : (Option Symbol) #false] #:lines [lines : (Listof Symbol) null]
           [texts : (U String (Listof String))] [font : (Option Font) #false]]
    (define smart-width : (Option Flonum) (if (or (infinite? max-width) (nan? max-width)) #false (real->double-flonum max-width)))
    (define-values (smart-height smart-emode)
      (cond [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (define body : String (if (list? texts) (string-join texts "\n") texts))
    
    (create-geometry-object geo:para
                            #:with [id (geo-draw-paragraph font fgsource bgsource)
                                       (geo-paragraph-extent font)
                                       geo-zero-pads]
                            body lines smart-width smart-height
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

(define geo-paragraph-extent : (-> (Option Font) Geo-Calculate-Extent)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:para?])
        (define-values (W H)
          (dc_paragraph_size (geo:string-body self) (geo-select-font-description alt-font default-font)
                             (geo:para-lines self) (geo:para-mwidth self) (geo:para-mheight self)
                             (geo:para-ident self) (geo:para-space self) (geo:para-wmode self) (geo:para-emode self)))
        (values W H #false)))))

(define geo-draw-text : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self cr x0 y0 width height]
      (when (geo:text? self)
        (dc_text cr x0 y0 width height
                 (geo:string-body self) (geo-select-font-description alt-font default-font) (geo:text-lines self)
                 (geo-select-font-source alt-fg) (geo-select-background-source alt-bg)
                 (stroke-paint->source* (geo:text-aline self)) (stroke-paint->source* (geo:text-cline self))
                 (stroke-paint->source* (geo:text-mline self))
                 (stroke-paint->source* (geo:text-bline self)) (stroke-paint->source* (geo:text-dline self)))))))

(define geo-draw-art-text : (-> (Option Font) Maybe-Stroke-Paint Maybe-Fill-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-font alt-outl alt-fill alt-bg]
    (λ [self cr x0 y0 flwidth flheight]
      (when (geo:art-text? self)
        (dc_art_text cr x0 y0 flwidth flheight
                     (geo:string-body self) (geo-select-font-description alt-font default-art-font) (geo:art-text-lines self)
                     (geo-select-stroke-paint alt-outl) (geo-select-fill-source alt-fill) (geo-select-background-source alt-bg))))))

(define geo-draw-paragraph : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self cr x0 y0 flwidth flheight]
      (when (geo:para? self)
        (dc_paragraph cr x0 y0 flwidth flheight
                      (geo:string-body self) (geo-select-font-description alt-font default-font) (geo:para-lines self)
                      (geo:para-mwidth self) (geo:para-mheight self)
                      (geo:para-ident self) (geo:para-space self) (geo:para-wmode self) (geo:para-emode self)
                      (geo-select-font-source alt-fg) (geo-select-background-source alt-bg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-select-font-description : (-> (Option Font) (-> Font) Font-Description)
  (lambda [alt-font fallback-font]
    (font-description (or alt-font (fallback-font)))))
