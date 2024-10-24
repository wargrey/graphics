#lang typed/racket/base

(provide (all-defined-out))
  
(require "../typed/cairo.rkt")
(require "../visual/ctype.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Vector-Stream-Surface-Make (-> Output-Port Nonnegative-Flonum Nonnegative-Flonum Positive-Index Cairo-Stream-Surface))

(define-type (Cairo-Vector-Stream-Write Master)
  (-> (U Path-String Output-Port) Nonnegative-Flonum Nonnegative-Flonum (Gairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Void))

(define-type (Cairo-Vector-Stream->Bytes Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Gairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Bytes))

(define-type (Open-Cairo-Input-Vector-Port Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Gairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Input-Port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) cairo-vector-stream-write
  : (-> (U Path-String Output-Port) Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
        (Gairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
        Void)
  (lambda [/dev/vecout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (if (output-port? /dev/vecout)
        (let* ([surface (λsurface /dev/vecout Width Height pool-size)]
               [cr (cairo_create surface)])
          (start-breakable-atomic)
          (λdc master cr x0 y0 flwidth flheight)
          (cairo_surface_flush surface)
          (cairo_surface_destroy surface)
          (end-breakable-atomic))
        (let ()
          (make-parent-directory* /dev/vecout)
          (call-with-output-file* /dev/vecout #:exists 'truncate/replace
            (λ [[/dev/pthout : Output-Port]]
              (cairo-vector-stream-write /dev/pthout λsurface
                                         Width Height pool-size
                                         λdc master x0 y0 flwidth flheight)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) make-cairo-vector-stream-bytes : (-> Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
                                                               (Gairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                               Bytes)
  (lambda [λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (define /dev/stdout (open-output-bytes '/dev/vecout))
    
    (cairo-vector-stream-write /dev/stdout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight)
    (get-output-bytes /dev/stdout)))

(define #:forall (Master) open-cairo-input-vector-stream : (-> Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
                                                               (Gairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                               Input-Port)
  (lambda [λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (define-values (/dev/stdin /dev/stdout) (make-pipe))

    (define ghostcat
      (thread
       (λ []
         (cairo-vector-stream-write /dev/stdout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))
