#lang typed/racket/base

(provide (all-defined-out))
  
(require "../cairo.rkt")
(require "../visual/ctype.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Vector-Stream-Surface-Make (-> Output-Port Nonnegative-Flonum Nonnegative-Flonum Positive-Index Cairo-Stream-Surface))
(define-type (Cairo-Vector-Stream-Surface-Draw! Master) (-> Master Cairo-Ctx Flonum Flonum Nonnegative-Flonum Nonnegative-Flonum Any))

(define-type (Cairo-Vector-Stream-Write Master)
  (-> (U Path-String Output-Port) Nonnegative-Flonum Nonnegative-Flonum
      (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
      Void))

(define-type (Cairo-Vector-Stream->Bytes Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum
      (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
      Bytes))

(define-type (Open-Cairo-Input-Vector-Port Master)
  (-> Nonnegative-Flonum Nonnegative-Flonum
      (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
      Input-Port))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) cairo-vector-stream-write
  : (-> (U Path-String Output-Port) Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
        (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
        Void)
  (lambda [/dev/vecout λsurface flwidth flheight pool-size λdc master x0 y0]
    (if (output-port? /dev/vecout)
        (let* ([surface (λsurface /dev/vecout flwidth flheight pool-size)]
               [cr (cairo_create surface)])
          (start-breakable-atomic)
          (λdc master cr x0 y0 flwidth flheight)
          (cairo_destroy cr)
          (cairo_surface_flush surface)
          (cairo_surface_destroy surface)
          (end-breakable-atomic))
        (let ()
          (make-parent-directory* /dev/vecout)
          (call-with-output-file* /dev/vecout #:exists 'truncate/replace
            (λ [[/dev/pthout : Output-Port]]
              (cairo-vector-stream-write /dev/pthout λsurface
                                         flwidth flheight pool-size λdc
                                         master x0 y0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (Master) make-cairo-vector-stream-bytes : (-> Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
                                                               (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
                                                               Bytes)
  (lambda [λsurface flwidth flheight pool-size λdc master x0 y0]
    (define /dev/stdout (open-output-bytes '/dev/vecout))
    
    (cairo-vector-stream-write /dev/stdout λsurface flwidth flheight pool-size λdc master x0 y0)
    (get-output-bytes /dev/stdout)))

(define #:forall (Master) open-cairo-input-vector-stream : (-> Cairo-Vector-Stream-Surface-Make Nonnegative-Flonum Nonnegative-Flonum Positive-Index
                                                               (Cairo-Vector-Stream-Surface-Draw! Master) Master Flonum Flonum
                                                               Input-Port)
  (lambda [λsurface flwidth flheight pool-size λdc master x0 y0]
    (define-values (/dev/stdin /dev/stdout) (make-pipe))

    (define ghostcat
      (thread
       (λ []
         (cairo-vector-stream-write /dev/stdout λsurface flwidth flheight pool-size λdc master x0 y0)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))
