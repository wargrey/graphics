#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/enumeration.rkt"))
(provide (all-from-out "digitama/parser/exn.rkt"))
(provide (all-from-out "digitama/parser/stream.rkt"))
(provide (all-from-out "digitama/unsafe/pixman.rkt"))
(provide (except-out (all-from-out "digitama/unsafe/convert.rkt")
                     create-argb-bitmap cairo-surface-shadow))

(require typed/racket/unsafe)

(require "digitama/enumeration.rkt")
(require "digitama/parser/exn.rkt")
(require "digitama/parser/stream.rkt")
(require "digitama/unsafe/convert.rkt")
(require "digitama/unsafe/pixman.rkt")

(unsafe-require/typed
 "digitama/unsafe/pangocairo.rkt"
 [cairo-create-image-surface (-> Flonum Flonum Flonum (Values Bitmap-Surface Positive-Index Positive-Index))]
 [cairo_image_surface_get_data (-> Bitmap-Surface Bytes)]
 [cairo_surface_mark_dirty (-> Bitmap-Surface Bytes)])

(require (for-syntax racket/base))

(define-syntax (define-read-bitmap stx)
  (syntax-case stx [lambda]
    [(_ #:-> Bitmap λsexp)
     (with-syntax ([read-bitmap (datum->syntax #'Bitmap (string->symbol (string-downcase (symbol->string (syntax-e #'Bitmap)))))])
       #'(define-read-bitmap read-bitmap #:-> Bitmap λsexp))]
    [(_ bitmap #:-> Bitmap (lambda [/dev/stdin density] sexp ...))
     (with-syntax ([read-bitmap (datum->syntax #'bitmap (string->symbol (string-append "read-" (symbol->string (syntax-e #'bitmap)))))])
       #'(define read-bitmap : (->* ((U Path-String Input-Port)) (#:density Positive-Flonum #:try-@2x? Boolean) Bitmap)
           (lambda [/dev/stdin #:density [density 1.0] #:try-@2x? [try-@2x? #false]]
             (cond [(input-port? /dev/stdin) sexp ...]
                   [else (let-values ([(path scale) (select-file@2x /dev/stdin density try-@2x?)])
                           (call-with-input-file* path #:mode 'binary
                             (λ [[stdin : Input-Port]]
                               (read-bitmap stdin #:density scale))))]))))]))

(define-syntax (create-bitmap stx)
  (syntax-case stx [:]
    [(_ [Bitmap convertor] filename density width height palettes depth argl ... decode)
     #'(let-values ([(surface fxwidth fxheight) (cairo-create-image-surface (exact->inexact width) (exact->inexact height) density)])
         (decode (cairo_image_surface_get_data surface) fxwidth fxheight)
         (cairo_surface_mark_dirty surface)
         (Bitmap convertor (cairo-surface-shadow surface) surface filename density width height palettes depth argl ...))]
    [(_ constructor filename density width height palettes depth argl ... decode)
     #'(create-bitmap [constructor #false] filename density width height palettes depth argl ... decode)]))

(define select-file@2x : (-> Path-String Positive-Flonum Boolean (Values Path-String Positive-Flonum))
  (lambda [src density try?]
    (cond [(not try?) (values src density)]
          [else (let* ([path.psd : String (if (string? src) src (path->string src))]
                       [path@2x.psd : String (regexp-replace #rx"([.][^.]*|)$" path.psd "@2x\\1")])
                  (cond [(not (file-exists? path@2x.psd)) (values path.psd density)]
                        [else (values path@2x.psd (+ density density))]))])))

(define bitmap-port-source : (-> Input-Port Symbol)
  (lambda [/dev/stdin]
    (string->unreadable-symbol (port-name /dev/stdin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-n:bytes : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (read-nbytes* /dev/stdin (read-uinteger /dev/stdin size))))

(define read-uinteger : (All (a) (case-> [Input-Port Natural -> Natural]
                                         [Input-Port Natural (-> Any Boolean : a) -> a]
                                         [Input-Port Natural (-> Any Boolean : a) Throw-Range-Error Symbol -> a]))
  (case-lambda
    [(/dev/stdin bsize) (integer-bytes->integer (read-bytes* /dev/stdin bsize) #false #true 0 bsize)]
    [(/dev/stdin bsize subinteger?) (assert (read-uinteger /dev/stdin bsize) subinteger?)]
    [(/dev/stdin bsize subinteger? throw src) (assert* (read-uinteger /dev/stdin bsize) subinteger? throw src)]))

(define read-signature : (-> Input-Port Bytes Boolean)
  (lambda [/dev/stdin signature]
    (define siglength : Index (bytes-length signature))
    
    (and (equal? signature (peek-nbytes* /dev/stdin siglength))
         (read-bytes siglength /dev/stdin)
         #true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-bytes* : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (if (bytes? bs) bs (throw-eof-error /dev/stdin))))

(define read-nbytes* : (-> Input-Port Natural Bytes)
  (lambda [/dev/stdin size]
    (define bs : (U Bytes EOF) (read-bytes size /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin)])))

(define peek-bytes* : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (if (bytes? bs) bs (throw-eof-error /dev/stdin))))

(define peek-nbytes* : (->* (Input-Port Natural) (Natural) Bytes)
  (lambda [/dev/stdin size [skip 0]]
    (define bs : (U Bytes EOF) (peek-bytes size skip /dev/stdin))
    (cond [(and (bytes? bs) (= (bytes-length bs) size)) bs]
          [else (throw-eof-error /dev/stdin)])))
