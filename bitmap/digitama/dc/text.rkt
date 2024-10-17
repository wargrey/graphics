#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/string)
(require racket/format)

(require geofun/font)
(require geofun/paint)

(require geofun/digitama/base)
(require geofun/digitama/font)
(require geofun/digitama/source)

(require geofun/digitama/unsafe/font)
(require geofun/digitama/unsafe/dc/text-layout)

(require "../convert.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-art-text : (->* (Any)
                               (Font #:stroke Maybe-Stroke-Paint #:fill Option-Fill-Paint #:background Option-Fill-Paint
                                     #:lines (Listof Symbol) #:density Positive-Flonum)
                               Bitmap)
  (lambda [#:stroke [outline (default-stroke-paint)] #:fill [pattern (default-fill-paint)] #:background [bgsource (default-background-paint)]
           #:margin [margin #false] #:lines [lines null] #:density [density (default-bitmap-density)]
           text [font (default-art-font)]]
    (define body : String (~a text))
    (define desc : Font-Description (font-description font))
    (define-values (width height) (font_get_text_extent desc body))
    
    (draw-bitmap dc_art_text #:with [width height density #true (stroke-paint->source* outline)]
                 [body desc lines]
                 [(fill-paint->source* pattern) (background->source* bgsource)])))

(define bitmap-text : (->* (Any)
                           (Font #:color Fill-Paint #:background Option-Fill-Paint #:lines (Listof Symbol)
                                 #:baseline Maybe-Stroke-Paint #:capline Maybe-Stroke-Paint #:meanline Maybe-Stroke-Paint
                                 #:ascent Maybe-Stroke-Paint #:descent Maybe-Stroke-Paint
                                 #:density Positive-Flonum)
                           Bitmap)
  (lambda [text [font (default-font)] #:color [ftsource (default-font-paint)] #:background [bgsource (default-background-paint)] #:lines [lines null]
                #:ascent [alsource #false] #:descent [dlsource #false] #:capline [clsource #false] #:meanline [mlsource #false]
                #:baseline [blsource #false] #:density [density (default-bitmap-density)]]
    (define body : String (~a text))
    (define desc : Font-Description (font-description font))
    (define-values (width height) (font_get_text_extent desc body))
    
    (draw-bitmap dc_text #:with [width height density #true]
                 body desc lines
                 (font-paint->source ftsource) (background->source* bgsource)
                 (stroke-paint->source* alsource) (stroke-paint->source* clsource) (stroke-paint->source* mlsource)
                 (stroke-paint->source* blsource) (stroke-paint->source* dlsource))))

(define bitmap-paragraph : (->* ((U String (Listof String)))
                                (Font #:color Fill-Paint #:background Option-Fill-Paint #:lines (Listof Symbol)
                                      #:max-width Real #:max-height Real #:indent Real #:spacing Real
                                      #:wrap-mode Paragraph-Wrap-Mode #:ellipsize-mode Paragraph-Ellipsize-Mode
                                      #:density Positive-Flonum)
                                Bitmap)
  (lambda [texts [font (default-font)] #:color [ftsource (default-font-paint)] #:background [bgsource (default-background-paint)] #:lines [lines null]
                 #:max-width [max-width +inf.0] #:max-height [max-height +inf.0] #:indent [indent 0.0] #:spacing [spacing 0.0]
                 #:wrap-mode [wrap-mode 'word-char] #:ellipsize-mode [ellipsize-mode 'end]
                 #:density [density (default-bitmap-density)]]
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
    (define-values (flwidth flheight draw-text?) (dc_paragraph_size body desc lines smart-width smart-height flindent flspacing fxwmode fxemode))
    
    (draw-bitmap dc_paragraph #:with [flwidth flheight density #true]
                 draw-text? body desc lines smart-width smart-height flindent flspacing fxwmode fxemode
                 (font-paint->source ftsource) (background->source* bgsource))))
