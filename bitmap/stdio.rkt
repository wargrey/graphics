#lang typed/racket/base

(provide (all-defined-out))
(provide Bitmap-Pixels exn-port-name)

(provide (all-from-out digimon/stdio))
(provide (all-from-out digimon/struct))
(provide (all-from-out digimon/checksum))
(provide (all-from-out digimon/enumeration))

(provide (all-from-out "digitama/self.rkt"))
(provide (all-from-out "digitama/convert.rkt"))
(provide (all-from-out "digitama/unsafe/pixman.rkt"))

(require digimon/stdio)
(require digimon/struct)
(require digimon/checksum)
(require digimon/enumeration)

(require geofun/digitama/unsafe/typed/cairo)
(require geofun/digitama/unsafe/surface/image)

(require "digitama/self.rkt")
(require "digitama/convert.rkt")
(require "digitama/unsafe/pixman.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Bitmap-Body-Decoder (-> Bitmap-Pixels Positive-Index Positive-Index Positive-Index Void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-read-bitmap stx)
  (syntax-case stx [lambda]
    [(_ #:-> Bitmap λsexp)
     (with-syntax ([read-bitmap (datum->syntax #'Bitmap (string->symbol (string-downcase (symbol->immutable-string (syntax-e #'Bitmap)))))])
       (syntax/loc stx (define-read-bitmap read-bitmap #:-> Bitmap λsexp)))]
    [(_ bitmap #:-> Bitmap (lambda [/dev/stdin density] sexp ...))
     (with-syntax ([read-bitmap (datum->syntax #'bitmap (string->symbol (string-append "read-" (symbol->immutable-string (syntax-e #'bitmap)))))])
       (syntax/loc stx
         (define read-bitmap : (->* ((U Path-String Input-Port)) (#:density Positive-Flonum #:try-@2x? Boolean) Bitmap)
           (lambda [/dev/stdin #:density [density 1.0] #:try-@2x? [try-@2x? #false]]
             (if (input-port? /dev/stdin)
                 (parameterize ([current-ioexn-input-port /dev/stdin])
                   sexp ...)

                 (let-values ([(path scale) (select-file@2x /dev/stdin density try-@2x?)])
                   (call-with-input-file* path #:mode 'binary
                     (λ [[stdin : Input-Port]]
                       (read-bitmap stdin #:density scale)))))))))]))

(define-syntax (create-bitmap stx)
  (syntax-case stx [:]
    [(_ [Bitmap convertor] filename density width height argl ... decoder)
     (syntax/loc stx
       (let*-values ([(surface fxwidth fxheight) (cairo-create-argb-image-surface (exact->inexact width) (exact->inexact height) 1.0)]
                     [(pixels) (cairo_image_surface_get_data* surface)]
                     [(stride) (max 1 (cairo_image_surface_get_stride surface))]
                     [(source) (if (input-port? filename) (bitmap-port-source filename) filename)]
                     [(decode) decoder])
         (cairo_surface_flush surface)
         (decode pixels fxwidth fxheight stride)
         (cairo_surface_mark_dirty surface)
         (Bitmap convertor
                 (cairo-image-shadow-size surface) surface source density
                 fxwidth fxheight argl ...)))]
    [(_ constructor filename density width height argl ... decode)
     (syntax/loc stx (create-bitmap [constructor bitmap-convert] filename density width height argl ... decode))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-file@2x : (-> Path-String Positive-Flonum Boolean (Values Path-String Positive-Flonum))
  (lambda [src density try?]
    (cond [(not try?) (values src density)]
          [else (let* ([path.psd : String (if (string? src) src (path->string src))]
                       [path@2x.psd : String (regexp-replace #rx"([.][^.]*|)$" path.psd "@2x\\1")])
                  (cond [(not (file-exists? path@2x.psd)) (values path.psd density)]
                        [else (values path@2x.psd (+ density density))]))])))

(define bitmap-port-source : (-> Input-Port Symbol)
  (lambda [/dev/stdin]
    (string->unreadable-symbol (exn-port-name /dev/stdin))))
