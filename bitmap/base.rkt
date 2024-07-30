#lang typed/racket

(provide (all-defined-out) Stroke-Paint Fill-Paint)

(require "digitama/source.rkt")
(require "digitama/misc.rkt")
(require "digitama/unsafe/visual/bitmap.rkt")

(require/provide "digitama/base.rkt" "digitama/stdio.rkt" "digitama/unsafe/convert.rkt")
(require/provide "constructor.rkt" "pixel.rkt" "composite.rkt" "resize.rkt")
(require/provide "color.rkt" "font.rkt" "paint.rkt" "misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-icon : (->* ((U Bitmap Path-String Input-Port))
                           ((-> Bitmap) #:dtrace String #:height Nonnegative-Real #:scale? Boolean)
                           Bitmap)
  (lambda [src [fallback #false] #:dtrace [tips #false] #:height [size #false] #:scale? [scale? #true]]
    (define (on-error-call-fallback [e : exn]) : Bitmap
      (define x.icon : Bitmap (if fallback (fallback) (error 'bitmap-icon "#:height (or size (toolbar-icon-height))")))
      (when (string? tips) (log-message (current-logger) 'warning (format "~a~n~a" tips (exn-message e)) x.icon #false))
      x.icon)
    (define raw.icon : Bitmap
      (with-handlers ([exn? on-error-call-fallback])
        (define src.icon : Bitmap
          (cond [(or (path? src) (string? src)) (read-bitmap src #:try-@2x? #true)]
                [(input-port? src) (read-bitmap src)]
                [else src]))
        (when (string? tips) (log-message (current-logger) 'info tips src.icon #false))
        src.icon))
    (cond [(or (false? size) (false? scale?)) raw.icon]
          [else (bitmap-scale raw.icon (/ size (bitmap-intrinsic-height raw.icon)))])))

(define bitmap : (->* ((U Path-String Input-Port)) (Positive-Flonum) Bitmap)
  ;;; https://drafts.csswg.org/css-images/#image-fragments
  (lambda [src [density (default-bitmap-density)]]
    (define (image-section [raw : Bitmap] [xywh : String] [hint : Symbol]) : Bitmap
      (match (regexp-match #px"^(\\d+),(\\d+),(\\d+),(\\d+)$" xywh)
        [(list _ (? string? (app string->number (? index? x))) (? string? (app string->number (? index? y)))
               (? string? (app string->number (? index? w))) (? string? (app string->number (? index? h))))
         (bitmap-section raw x y w h)]
        [_ (raise-user-error hint "malformed fragment")]))
    (cond [(input-port? src) (read-bitmap src #:backing-scale density)]
          [(not (regexp-match? #px"[?]id=" src))
           (define (read-image.bmp [src.bmp : String]) : Bitmap (read-bitmap src.bmp #:backing-scale density))
           (match (string-split (if (path? src) (path->string src) src) "#xywh=")
             [(list src.rkt xywh) (image-section (read-image.bmp src.rkt) xywh 'read-image)]
             [(list src.rkt) (read-image.bmp src.rkt)]
             [_ (raise-user-error 'read-image "too many fragment")])]
          [else #| path?id=binding#xywh=x,y,w,h |#
           (define (read-image.rkt [src.rkt : String] [id : Symbol]) : Bitmap
             (define raw (require-image src.rkt id density))
             (cond [(bitmap? raw) raw]
                   [else (error 'require-image "contract violation: received ~s" raw)]))
           (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#xywh=)")
             [(list src.rkt id xywh) (image-section (read-image.rkt src.rkt (string->symbol id)) xywh 'require-image)]
             [(list src.rkt id) (read-image.rkt src.rkt (string->symbol id))]
             [_ (raise-user-error 'require-image "too many fragment")])])))

(define sprite : (->* ((U Path-String Input-Port)) (Positive-Flonum) (Listof Bitmap))
  (lambda [src [density (default-bitmap-density)]]
    (define (image-disassemble [raw : Bitmap] [grid : String] [hint : Symbol]) : (Listof Bitmap)
      (match (regexp-match #px"^(\\d+),(\\d+)$" grid)
        [(list _ (? string? (app string->number (? exact-positive-integer? cols)))
               (? string? (app string->number (? exact-positive-integer? rows))))
         (bitmap->sprite raw cols rows)]
        [_ (raise-user-error hint "malformed fragments")]))
    (cond [(input-port? src) (list (read-bitmap src #:backing-scale density))]
          [(not (regexp-match? #px"[?]id=" src))
           (define (read-sprite.bmp [src.bmp : String]) : Bitmap (read-bitmap src.bmp #:backing-scale density))
           (match (string-split (if (path? src) (path->string src) src) "#grids=")
             [(list src.rkt xywh) (image-disassemble (read-sprite.bmp src.rkt) xywh 'read-sprite)]
             [(list src.rkt) (list (read-sprite.bmp src.rkt))]
             [_ (raise-user-error 'read-sprite "too many fragments")])]
          [else #| path?id=binding |#
           (define (read-sprite.rkt [src.rkt : String] [id : Symbol] [grid : (Option String)]) : (Listof Bitmap)
             (define raw (require-image src.rkt id density))
             (cond [(list? raw) (filter-map (λ [v] (and (bitmap? v) v)) raw)]
                   [(not (bitmap? raw)) (error 'require-sprite "contract violation: received ~s" raw)]
                   [(string? grid) (image-disassemble raw grid 'require-sprite)]
                   [else (list raw)]))
           (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#grids=)")
             [(list src.rkt id grid) (read-sprite.rkt src.rkt (string->symbol id) grid)]
             [(list src.rkt id) (read-sprite.rkt src.rkt (string->symbol id) #false)]
             [_ (raise-user-error 'require-sprite "too many queries")])])))

(define bitmap->sprite : (->* (Bitmap) (Positive-Integer Positive-Integer) (Listof Bitmap))
  (lambda [bmp [cols 1] [rows 1]]
    (define-values (width height)
      (values (/ (bitmap-intrinsic-width bmp) cols)
              (/ (bitmap-intrinsic-height bmp) rows)))
    (reverse (for*/fold ([sprite : (Listof Bitmap) null])
                        ([y (in-range rows)] [x (in-range cols)])
               (cons (bitmap-copy bmp (* x width) (* y height) width height) sprite)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define bitmap-save : (->* (Bitmap (U Path-String Output-Port)) (#:format Symbol) Void)
  (lambda [bmp /dev/bmpout #:format [format 'png]]
    (bitmap-surface-save (bitmap<%>-surface bmp) /dev/bmpout format (bitmap-density bmp))))
