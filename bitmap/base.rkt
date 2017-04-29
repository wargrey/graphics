#lang typed/racket

(provide (all-defined-out))
(provide (all-from-out "digitama/digicore.rkt"))
(provide (except-out (all-from-out typed/images/icons) default-icon-backing-scale default-icon-height))

(require "digitama/misc.rkt")

(require typed/images/icons)

(require/provide "digitama/digicore.rkt")
(require/provide "constructor.rkt" "combiner.rkt" "resize.rkt")
(require/provide "color.rkt" "font.rkt" "background.rkt" "misc.rkt")

(define build-bitmap : (-> Nonnegative-Real Nonnegative-Real (-> Fixnum Fixnum (Values Real Real Real Real))
                           [#:backing-scale Positive-Real] [#:alpha-multiplied? Boolean] Bitmap)
  (lambda [width height fargb #:backing-scale [density (default-bitmap-density)] #:alpha-multiplied? [/alpha? #false]]
    (define-values (w h) (values (exact-floor (* width density)) (exact-floor (* height density))))
    (define size : Integer (* w h 4))
    (define buffer : Bytes (make-bytes size))
    (define argb->rgb : (-> Real Real Real Real (Values Real Real Real))
      (cond [(not /alpha?) (λ [a r g b] (values r g b))]
            [else (λ [a r g b] (cond [(zero? a) (values r g b)]
                                     [else (values (/ r a) (/ g a) (/ b a))]))]))
    (let y-loop : Void ([y : Fixnum 0])
      (when (fx< y h)
        (let x-loop : Void ([x : Fixnum 0])
          (when (fx< x w)
            (define idx : Fixnum (fx* (fx+ (fx* y w) x) 4))
            (define-values (a r0 g0 b0) (fargb x y))
            (define-values (r g b) (argb->rgb a r0 g0 b0))
            (bytes-set! buffer (fx+ idx 0) (fxmin (fxmax (exact-round (* a #xFF)) #x00) #xFF))
            (bytes-set! buffer (fx+ idx 1) (fxmin (fxmax (exact-round (* r #xFF)) #x00) #xFF))
            (bytes-set! buffer (fx+ idx 2) (fxmin (fxmax (exact-round (* g #xFF)) #x00) #xFF))
            (bytes-set! buffer (fx+ idx 3) (fxmin (fxmax (exact-round (* b #xFF)) #x00) #xFF))
            (x-loop (fx+ x 1))))
        (y-loop (fx+ y 1))))
    (define bmp (make-bitmap (max 1 (exact-ceiling width)) (max 1 (exact-ceiling height)) #:backing-scale density))
    (send bmp set-argb-pixels 0 0 w h buffer)
    bmp))

(define bitmap-icon : (->* ((U Bitmap Path-String Input-Port))
                           ((-> Bitmap) #:dtrace String #:height Nonnegative-Real #:scale? Boolean)
                           Bitmap)
  (lambda [src [fallback #false] #:dtrace [tips #false] #:height [size #false] #:scale? [scale? #true]]
    (define (on-error-call-fallback [e : exn]) : Bitmap
      (define x.icon : Bitmap (if fallback (fallback) (x-icon #:height (or size (toolbar-icon-height)))))
      (when (string? tips) (log-message (current-logger) 'warning (format "~a~n~a" tips (exn-message e)) x.icon #false))
      x.icon)
    (define raw.icon : Bitmap
      (with-handlers ([exn? on-error-call-fallback])
        (define src.icon : Bitmap
          (cond [(or (path? src) (string? src)) (read-bitmap src #:try-@2x? #true)]
                [(input-port? src) (read-bitmap src)]
                [else (if (send src ok?) src (error 'bitmap-icon "invalid bitmap"))]))
        (when (string? tips) (log-message (current-logger) 'info tips src.icon #false))
        src.icon))
    (cond [(or (false? size) (false? scale?)) raw.icon]
          [else (bitmap-scale raw.icon (/ size (send raw.icon get-height)))])))

(define bitmap : (->* ((U Path-String Input-Port)) (Positive-Real) Bitmap)
  ;;; https://drafts.csswg.org/css-images/#image-fragments
  (lambda [src [density (default-bitmap-density)]]
    (define (image-section [raw : Bitmap] [xywh : String] [hint : Symbol]) : Bitmap
      (cond [(not (send raw ok?)) the-invalid-image]
            [else (match (regexp-match #px"^(\\d+),(\\d+),(\\d+),(\\d+)$" xywh)
                    [(list _ (? string? (app string->number (? index? x))) (? string? (app string->number (? index? y)))
                           (? string? (app string->number (? index? w))) (? string? (app string->number (? index? h))))
                     (bitmap-section raw x y w h)]
                    [_ (raise-user-error hint "malformed fragment")])]))
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
             (cond [(bitmap%? raw) raw]
                   [else (error 'require-image "contract violation: received ~s" raw)]))
           (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#xywh=)")
             [(list src.rkt id xywh) (image-section (read-image.rkt src.rkt (string->symbol id)) xywh 'require-image)]
             [(list src.rkt id) (read-image.rkt src.rkt (string->symbol id))]
             [_ (raise-user-error 'require-image "too many fragment")])])))

(define sprite : (->* ((U Path-String Input-Port)) (Positive-Real) (Listof Bitmap))
  (lambda [src [density (default-bitmap-density)]]
    (define (image-disassemble [raw : Bitmap] [grid : String] [hint : Symbol]) : (Listof Bitmap)
      (cond [(not (send raw ok?)) null]
            [else (match (regexp-match #px"^(\\d+),(\\d+)$" grid)
                    [(list _ (? string? (app string->number (? exact-positive-integer? cols)))
                           (? string? (app string->number (? exact-positive-integer? rows))))
                     (bitmap->sprite raw cols rows)]
                    [_ (raise-user-error hint "malformed fragments")])]))
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
             (cond [(list? raw) (filter-map (λ [v] (and (bitmap%? v) v)) raw)]
                   [(not (bitmap%? raw)) (error 'require-sprite "contract violation: received ~s" raw)]
                   [(string? grid) (image-disassemble raw grid 'require-sprite)]
                   [else (list raw)]))
           (match (string-split (if (path? src) (path->string src) src) #px"([?]id=)|(#grids=)")
             [(list src.rkt id grid) (read-sprite.rkt src.rkt (string->symbol id) grid)]
             [(list src.rkt id) (read-sprite.rkt src.rkt (string->symbol id) #false)]
             [_ (raise-user-error 'require-sprite "too many queries")])])))

(define bitmap->sprite : (->* (Bitmap) (Positive-Integer Positive-Integer) (Listof Bitmap))
  (lambda [bmp [cols 1] [rows 1]]
    (define-values (width height) (values (/ (send bmp get-width) cols) (/ (send bmp get-height) rows)))
    (reverse (for*/fold ([sprite : (Listof Bitmap) null])
                        ([y (in-range rows)] [x (in-range cols)])
               (cons (bitmap-copy bmp (* x width) (* y height) width height) sprite)))))
