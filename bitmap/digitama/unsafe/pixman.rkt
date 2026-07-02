#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)
(require geofun/digitama/unsafe/typed/c)

(module unsafe racket/base
  (provide (all-defined-out))
  
  (require ffi/unsafe)
  (require racket/unsafe/ops)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define-values (A R G B) (if (system-big-endian?) (values 0 1 2 3) (values 3 2 1 0)))
  (define RGBA (vector-immutable R G B A))

  (define pix-set-rgba-channel-byte!
    (lambda [pixels idx channel value]
      (ptr-set! pixels _ubyte
                (unsafe-fx+ idx (unsafe-vector*-ref RGBA channel))
                value)))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define pix-zero?
    (lambda [pixels idx]
      (and (unsafe-fx= (ptr-ref pixels _ubyte idx) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 1)) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 2)) 0)
           (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx 3)) 0))))

  (define pix-alpha-zero?
    (lambda [pixels idx [a A]]
      (unsafe-fx= (ptr-ref pixels _ubyte (unsafe-fx+ idx a)) 0)))
  
  (define pix-zero!
    (lambda [pixels idx]
      (memset pixels idx 0 4 _ubyte)))

  (define pix-fill!
    (case-lambda
      [(pixels idx value count)
       (memset pixels idx value count _ubyte)]
      [(pixels value count)
       (memset pixels 0 value count _ubyte)]))

  (define pix-randomize! ; for test
    (lambda [pixels width height [n 0]]
      (when (and (unsafe-fx> width 0) (unsafe-fx> height 0))
        (define stride (unsafe-fx* width 4))
        (define times
          (cond [(> n 0) n]
                [else (unsafe-fl->fx
                       (unsafe-flround
                        (unsafe-fl* (unsafe-fl* (unsafe-fx->fl width) (unsafe-fx->fl height))
                                    0.618)))]))
        
        (let randomize ([n times])
          (when (> n 0)
            (define-values (w h) (values (random width) (random height)))
            
            (pix-set-argb-bytes! pixels
                                 (unsafe-fx+ (unsafe-fx* stride h)
                                             (unsafe-fx* w 4))
                                 #xFF (random #x100) (random #x100) (random #x100))
            (randomize (unsafe-fx- n 1)))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define pix-alpha-ref
    (lambda [pixels idx]
      (ptr-ref pixels _ubyte (unsafe-fx+ idx A))))

  ;;; WARNING: the color component value cannot be greater than alpha if it is properly scaled
  (define pix-set-argb-flonums!
    (lambda [pixels idx a r g b]
      (define alpha (alpha-multiplied-flonum->byte a #x100))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx A) alpha)
      (ptr-set! pixels _ubyte (unsafe-fx+ idx R) (alpha-multiplied-flonum->byte r alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx G) (alpha-multiplied-flonum->byte g alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx B) (alpha-multiplied-flonum->byte b alpha))))
  
  (define pix-set-argb-bytes!
    (lambda [pixels idx alpha r g b]
      (ptr-set! pixels _ubyte (unsafe-fx+ idx A) alpha)
      (safe-set-rgb-bytes pixels idx alpha r g b)))
  
  (define pix-get-argb-bytes
    (lambda [pixels idx]
      (define-values (r g b) (pix-get-rgb-bytes pixels idx))
      (values (ptr-ref pixels _ubyte (unsafe-fx+ idx A)) r g b)))
  
  (define pix-set-rgb-bytes!
    (lambda [pixels idx r g b]
      (define alpha (ptr-ref pixels _ubyte (unsafe-fx+ idx A)))
      (safe-set-rgb-bytes pixels idx alpha r g b)))
  
  (define pix-get-rgb-bytes
    (lambda [pixels idx]
      (values (ptr-ref pixels _ubyte (unsafe-fx+ idx R))
              (ptr-ref pixels _ubyte (unsafe-fx+ idx G))
              (ptr-ref pixels _ubyte (unsafe-fx+ idx B)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (alpha-multiplied-flonum->byte v alpha)
    (unsafe-fxmax #x00
                  (unsafe-fxmin alpha
                                (unsafe-fl->fx
                                 (unsafe-flround
                                  (unsafe-fl* v 255.0))))))
  
  (define safe-set-rgb-bytes
    (lambda [pixels idx alpha r g b]
      (ptr-set! pixels _ubyte (unsafe-fx+ idx R) (unsafe-fxmin r alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx G) (unsafe-fxmin g alpha))
      (ptr-set! pixels _ubyte (unsafe-fx+ idx B) (unsafe-fxmin b alpha)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 (submod "." unsafe)
 [pix-set-rgba-channel-byte! (-> Bitmap-Pixels Natural Byte Byte Void)]
 
 [pix-randomize! (->* (Bitmap-Pixels Natural Natural) (Index) Void)]
 [pix-fill! (case-> [Bitmap-Pixels Natural Byte Natural -> Void]
                    [Bitmap-Pixels Byte Natural -> Void])]
 
 [pix-zero! (-> Bitmap-Pixels Natural Void)]
 [pix-zero? (-> Bitmap-Pixels Natural Boolean)]
 [pix-alpha-zero? (-> Bitmap-Pixels Natural Boolean)]

 [pix-alpha-ref (-> Bitmap-Pixels Natural Byte)]
 [pix-get-argb-bytes (-> Bitmap-Pixels Natural (Values Byte Byte Byte Byte))]
 [pix-get-rgb-bytes (-> Bitmap-Pixels Natural (Values Byte Byte Byte))]

 [pix-set-argb-flonums! (-> Bitmap-Pixels Natural Flonum Flonum Flonum Flonum Void)]
 [pix-set-argb-bytes! (-> Bitmap-Pixels Natural Byte Byte Byte Byte Void)]
 [pix-set-rgb-bytes! (-> Bitmap-Pixels Natural Byte Byte Byte Void)])