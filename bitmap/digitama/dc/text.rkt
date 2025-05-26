#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/string)
(require racket/format)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/font)
(require geofun/digitama/markup)
(require geofun/digitama/paint/source)

(require geofun/digitama/unsafe/font)
(require geofun/digitama/unsafe/dc/text-layout)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-art-text
  (lambda [#:stroke [outline : Maybe-Stroke-Paint (default-stroke-paint)] #:fill [pattern : Option-Fill-Paint (default-fill-paint)]
           #:background [bgsource : Option-Fill-Paint (default-background-paint)]
           #:lines [lines : (Listof Geo-Text-Line) null] #:alignment [align : Geo-Text-Alignment 'left]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [text : Any] [font : Font (default-art-font)]] : Bitmap
    (define body : String (~a text))
    (define desc : Font-Description (font-description font))
    (define-values (width height) (font_get_text_extent desc body))
    
    (draw-bitmap dc_art_text #:with [width height density #true (stroke-paint->source* outline)]
                 [body desc lines align]
                 [(fill-paint->source* pattern) (background->source* bgsource)])))

(define bitmap-text
  (lambda [#:color [ftsource : Option-Fill-Paint (default-font-paint)] #:background [bgsource : Option-Fill-Paint (default-background-paint)]
           #:lines [lines : (Listof Geo-Text-Line) null] #:alignment [align : Geo-Text-Alignment 'left]
           #:baseline [blsource : Maybe-Stroke-Paint #false]
           #:ascent [alsource : Maybe-Stroke-Paint #false] #:descent [dlsource : Maybe-Stroke-Paint #false]
           #:capline [clsource : Maybe-Stroke-Paint #false] #:meanline [mlsource : Maybe-Stroke-Paint #false]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [text : Any] [font : (Option Font) #false]] : Bitmap
    (define body : String (~a text))
    (define desc : Font-Description (font-description (or font (default-font))))
    (define-values (width height) (font_get_text_extent desc body))
    
    (draw-bitmap dc_text #:with [width height density #true]
                 body desc lines align
                 (font-paint->source ftsource) (background->source* bgsource)
                 (stroke-paint->source* alsource) (stroke-paint->source* clsource) (stroke-paint->source* mlsource)
                 (stroke-paint->source* blsource) (stroke-paint->source* dlsource))))

(define bitmap-paragraph
  (lambda [#:color [ftsource : Fill-Paint (default-font-paint)] #:background [bgsource : Option-Fill-Paint (default-background-paint)]
           #:lines [lines : (Listof Geo-Text-Line) null] #:alignment [align : Geo-Text-Alignment 'left]
           #:max-width [max-width : Real +inf.0] #:max-height [max-height : Real +inf.0]
           #:indent [indent : Real 0.0] #:spacing [spacing : Real 0.0]
           #:wrap-mode [wrap-mode : Paragraph-Wrap-Mode 'word-char] #:ellipsize-mode [ellipsize-mode : Paragraph-Ellipsize-Mode 'end]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [texts : (U String (Listof String))] [font : Font (default-font)]] : Bitmap
    (define smart-width : (Option Flonum) (if (or (infinite? max-width) (nan? max-width)) #false (real->double-flonum max-width)))
    (define-values (smart-height smart-emode)
      (cond [(or (infinite? max-height) (nan? max-height)) (values -1 'none)]
            [(negative? max-height) (values (exact-round max-height) ellipsize-mode)]
            [else (values (real->double-flonum max-height) ellipsize-mode)]))
    (define-values (flindent flspacing) (values (real->double-flonum indent) (real->double-flonum spacing)))
    (define fxwmode (paragraph-wrap-mode->integer wrap-mode raise-argument-error))
    (define fxemode (paragraph-ellipsize-mode->integer smart-emode raise-argument-error))
    (define body : String (if (list? texts) (string-join texts "\n") texts))
    (define desc : Font-Description (font-description font))
    (define-values (flwidth flheight) (dc_paragraph_size body desc lines align smart-width smart-height flindent flspacing fxwmode fxemode))
    
    (draw-bitmap dc_paragraph #:with [flwidth flheight density #true]
                 body desc lines align
                 smart-width smart-height flindent flspacing fxwmode fxemode
                 (font-paint->source ftsource) (background->source* bgsource))))

(define bitmap-markup
  (lambda [#:color [ftsource : Option-Fill-Paint (default-font-paint)] #:background [bgsource : Option-Fill-Paint (default-background-paint)]
           #:error-color [errfg : Option-Fill-Paint #false] #:error-background [errbg : Option-Fill-Paint #false]
           #:lines [lines : (Listof Geo-Text-Line) null] #:alignment [align : Geo-Text-Alignment 'left]
           #:density [density : Positive-Flonum (default-bitmap-density)]
           [text : Any] [font : Font (default-font)]] : Bitmap
    (define markup : Bytes (dc-markup-datum->text text))
    (define desc : Font-Description (font-description font))
    (define-values (width height plain) (dc_markup_parse markup desc lines align))

    (cond [(string? plain)
           (draw-bitmap dc_markup #:with [width height density #true] markup desc lines align
                        (font-paint->source ftsource) (background->source* bgsource))]
          [(or errfg errbg)
           (bitmap-text #:lines lines #:color errfg #:background errbg #:alignment align #:density density
                        (bytes-append plain #"\n" markup) font)]
          [else (error 'bitmap-markup "~a\n~a" plain markup)])))
