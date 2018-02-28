#lang typed/racket/base

(provide (all-defined-out))

(require "require.rkt")

(module unsafe racket/base
  (provide (all-defined-out))

  (require racket/unsafe/ops)

  (define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))

  (define pixels-argb-ref
    (lambda [pixels idx big-endian?]
      (integer-bytes->integer pixels #false big-endian? idx (unsafe-fx+ idx 4))))
  
  (define pixels-alpha-ref
    (lambda [pixels idx]
      (unsafe-bytes-ref pixels (unsafe-fx+ idx A))))
  
  ;;; WARNING: the color component value cannot be greater than alpha if it is properly scaled
  (define pixels-set-argb-reals
    (lambda [pixels idx a r g b]
      (define alpha (alpha-multiplied-real->byte a 255))
      (unsafe-bytes-set! pixels (unsafe-fx+ idx A) alpha)
      (unsafe-bytes-set! pixels (unsafe-fx+ idx R) (alpha-multiplied-real->byte r alpha))
      (unsafe-bytes-set! pixels (unsafe-fx+ idx G) (alpha-multiplied-real->byte g alpha))
      (unsafe-bytes-set! pixels (unsafe-fx+ idx B) (alpha-multiplied-real->byte b alpha))))
  
  (define pixels-set-argb-bytes
    (lambda [pixels idx alpha r g b]
      (unsafe-bytes-set! pixels (unsafe-fx+ idx A) alpha)
      (pixels-set-rgb-bytes/safe pixels idx alpha r g b)))
  
  (define pixels-get-argb-bytes
    (lambda [pixels idx]
      (define-values (r g b) (pixels-get-rgb-bytes pixels idx))
      (values (unsafe-bytes-ref pixels (unsafe-fx+ idx A)) r g b)))

  (define pixels-set-rgb-bytes
    (lambda [pixels idx r g b]
      (define alpha (unsafe-bytes-ref pixels (unsafe-fx+ idx A)))
      (pixels-set-rgb-bytes/safe pixels idx alpha r g b)))
  
  (define pixels-get-rgb-bytes
    (lambda [pixels idx]
      (values (unsafe-bytes-ref pixels (unsafe-fx+ idx R))
              (unsafe-bytes-ref pixels (unsafe-fx+ idx G))
              (unsafe-bytes-ref pixels (unsafe-fx+ idx B)))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (alpha-multiplied-real->byte v alpha)
    (unsafe-fxmax #x00
                  (unsafe-fxmin alpha
                                (unsafe-fl->fx
                                 (unsafe-flround
                                  (unsafe-fl* (real->double-flonum v) 255.0))))))

  (define pixels-set-rgb-bytes/safe
    (lambda [pixels idx alpha r g b]
      (unsafe-bytes-set! pixels (unsafe-fx+ idx R) (unsafe-fxmin r alpha))
      (unsafe-bytes-set! pixels (unsafe-fx+ idx G) (unsafe-fxmin g alpha))
      (unsafe-bytes-set! pixels (unsafe-fx+ idx B) (unsafe-fxmin b alpha)))))

(unsafe/require/provide
 (submod "." unsafe)
 [pixels-set-argb-reals (-> Bytes Integer Real Real Real Real Void)]
 [pixels-set-argb-bytes (-> Bytes Integer Byte Byte Byte Byte Void)]
 [pixels-get-argb-bytes (-> Bytes Integer (Values Byte Byte Byte Byte))]
 [pixels-set-rgb-bytes (-> Bytes Integer Byte Byte Byte Void)]
 [pixels-get-rgb-bytes (-> Bytes Integer (Values Byte Byte Byte))])
