#lang typed/racket/base

(provide (all-defined-out))

(require racket/file)

(require "../typed/cairo.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Vector-Stream-Surface-Make (-> Output-Port Nonnegative-Flonum Nonnegative-Flonum Positive-Index Cairo-Stream-Surface))

(define-type (Cairo-Vector-Stream-Write Master)
  (-> (U Path-String Output-Port) Nonnegative-Flonum Nonnegative-Flonum (Cairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Void))

(define-type (Cairo-Vector-Stream->Bytes Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Cairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Bytes))

(define-type (Open-Cairo-Input-Vector-Port Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum Positive-Flonum Boolean (Cairo-Surface-Draw! Master)
      Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
      Input-Port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) cairo-vector-stream-write
  : (-> (U Path-String Output-Port) Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
        (Cairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
        Void)
  (lambda [/dev/vecout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (if (output-port? /dev/vecout)
        (let* ([surface (λsurface /dev/vecout Width Height pool-size)]
               [vec-cr (cairo_create surface)])
          ; WARNING: Either `cairo_destroy` or `cairo_surface_finish` is required to flush the writer
          (start-breakable-atomic)
          (λdc master vec-cr x0 y0 flwidth flheight)
          (cairo_surface_flush surface)
          (cairo_surface_finish surface)
          (cairo_surface_destroy surface)
          (cairo_destroy vec-cr)
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
                                                               (Cairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                               Bytes)
  (lambda [λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (define /dev/stdout (open-output-bytes '/dev/vecout))
    
    (cairo-vector-stream-write /dev/stdout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight)
    (get-output-bytes /dev/stdout)))

(define #:forall (Master) open-cairo-input-vector-stream : (-> Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
                                                               (Cairo-Surface-Draw! Master) Master Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum
                                                               Input-Port)
  (lambda [λsurface Width Height pool-size λdc master x0 y0 flwidth flheight]
    (define-values (/dev/stdin /dev/stdout) (make-pipe))

    (define ghostcat
      (thread
       (λ []
         (cairo-vector-stream-write /dev/stdout λsurface Width Height pool-size λdc master x0 y0 flwidth flheight)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))
