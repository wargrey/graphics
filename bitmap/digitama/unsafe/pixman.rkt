#lang typed/racket/base

(provide (all-defined-out))

(module unsafe racket/base
  (provide (all-defined-out))

  (require ffi/unsafe)
  (require racket/unsafe/ops)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))

  (define pixel-zero?
    (lambda [pixels idx]
      (and (unsafe-fx= (ptr-ref pixels _ubyte idx) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 1)) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 2)) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 3)) 0))))
  
  (define pixel-alpha-zero?
    (lambda [pixels idx [a A]]
      (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx a)) 0)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define pixels-alpha-ref
    (lambda [pixels idx]
      (ptr-ref pixels _ubyte (unsafe-fx+ idx A))))
  
  ;;; WARNING: the color component value cannot be greater than alpha if it is properly scaled
  (define pixels-set-argb-flonums
    (lambda [pixels idx a r g b]
      (define alpha (alpha-multiplied-flonum->byte a 255))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx A) alpha)
      (ptr-set! pixels _ubyte (unsafe-fx+ idx R) (alpha-multiplied-flonum->byte r alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx G) (alpha-multiplied-flonum->byte g alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx B) (alpha-multiplied-flonum->byte b alpha))))
  
  (define pixels-set-argb-bytes
    (lambda [pixels idx alpha r g b]
      (ptr-set! pixels _ubyte (unsafe-fx+ idx A) alpha)
      (pixels-set-rgb-bytes/safe pixels idx alpha r g b)))
  
  (define pixels-get-argb-bytes
    (lambda [pixels idx]
      (define-values (r g b) (pixels-get-rgb-bytes pixels idx))
      (values (ptr-ref pixels _ubyte (unsafe-fx+ idx A)) r g b)))

  (define pixels-set-rgb-bytes
    (lambda [pixels idx r g b]
      (define alpha (ptr-ref pixels _ubyte (unsafe-fx+ idx A)))
      (pixels-set-rgb-bytes/safe pixels idx alpha r g b)))
  
  (define pixels-get-rgb-bytes
    (lambda [pixels idx]
      (values (ptr-ref pixels _ubyte (unsafe-fx+ idx R))
              (ptr-ref pixels _ubyte (unsafe-fx+ idx G))
              (ptr-ref pixels _ubyte (unsafe-fx+ idx B)))))

  (define pixels-zero
    (lambda [pixels idx]
      (memset pixels idx 0 4 _ubyte)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (alpha-multiplied-flonum->byte v alpha)
    (unsafe-fxmax #x00
                  (unsafe-fxmin alpha
                                (unsafe-fl->fx
                                 (unsafe-flround
                                  (unsafe-fl* v 255.0))))))

  (define pixels-set-rgb-bytes/safe
    (lambda [pixels idx alpha r g b]
      (ptr-set! pixels _ubyte (unsafe-fx+ idx R) (unsafe-fxmin r alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx G) (unsafe-fxmin g alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx B) (unsafe-fxmin b alpha)))))
