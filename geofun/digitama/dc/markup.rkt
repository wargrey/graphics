#lang typed/racket/base

;;; https://docs.gtk.org/Pango/pango_markup.html

(provide (all-defined-out))

(require "text.rkt")
(require "../../font.rkt")

(require "../self.rkt")
(require "../paint.rkt")
(require "../convert.rkt")
(require "../richtext/markup.rkt")

(require "../unsafe/dc/text-layout.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct geo:markup geo:string
  ([raw : Bytes])
  #:type-name Geo:Markup
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-markup
  (lambda [#:id [id : (Option Symbol) #false] #:lines [lines : (Listof Geo-Text-Line) null] #:alignment [align : Geo-Text-Alignment 'left]
           #:color [fgsource : Option-Fill-Paint #false] #:background [bgsource : Maybe-Fill-Paint (void)]
           #:error-color [errfg : Option-Fill-Paint #false] #:error-background [errbg : Option-Fill-Paint #false]
           [text : Geo-Markup-Datum] [font : (Option Font) #false]] : (U Geo:Markup Geo:Text)
    (define markup : Bytes (geo-markup-datum->text text))
    (define-values (plain has-attrs?) (dc_markup_plain_text* markup))

    (cond [(string? plain)
           (if (not has-attrs?)
               (create-geometry-object geo:text
                                       #:with [id (geo-draw-text font fgsource bgsource)
                                                  (geo-text-extent font)
                                                  geo-zero-pads]
                                       plain lines align #false #false #false #false #false)
               (create-geometry-object geo:markup
                                       #:with [id (geo-draw-markup font fgsource bgsource)
                                                  (geo-markup-extent font)
                                                  geo-zero-pads]
                                       plain lines align markup))]
          [(or errfg errbg)
           (geo-text (bytes-append plain #"\n" markup) font #:id id #:color errfg #:background errbg #:alignment align)]
          [(or id) (error 'geo-markup "~a: ~a\n~a" id plain markup)]
          [else    (error 'geo-markup "~a\n~a" plain markup)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-markup-extent : (-> (Option Font) Geo-Calculate-Extent)
  (lambda [alt-font]
    (λ [self]
      (with-asserts ([self geo:markup?])
        (define font-desc (geo-select-font-description alt-font default-font))
        (define-values (W H _) (dc_markup_parse (geo:markup-raw self) font-desc (geo:string-lines self) (geo:string-alignment self)))
        (values W H #false)))))

(define geo-draw-markup : (-> (Option Font) Option-Fill-Paint Maybe-Fill-Paint Geo-Surface-Draw!)
  (lambda [alt-font alt-fg alt-bg]
    (λ [self cr x0 y0 flwidth flheight]
      (when (geo:markup? self)
        (dc_markup cr x0 y0 flwidth flheight
                   (geo:markup-raw self) (geo-select-font-description alt-font default-font)
                   (geo:string-lines self) (geo:string-alignment self)
                   (geo-select-font-source alt-fg) (geo-select-background-source alt-bg))))))
