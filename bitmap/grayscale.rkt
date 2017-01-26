#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/bitmap.rkt")

(define bitmap-grayscale : (-> Bitmap (-> Byte Byte Byte Byte) Bitmap)
  (lambda [bmp rgb->gray]
    (define-values (w h bs) (values (send bmp get-width) (send bmp get-height) (send bmp get-backing-scale)))
    (define size : Integer (exact-ceiling (* w h bs bs 4)))
    (define buffer : Bytes (make-bytes size))
    (send bmp get-argb-pixels 0 0 w h buffer)
    (let scale ([r : Integer 1])
      (when (fx< r size)
        (define-values (g b) (values (fx+ r 1) (fx+ r 2)))
        (define gray : Byte (rgb->gray (bytes-ref buffer r) (bytes-ref buffer g) (bytes-ref buffer b)))
        (bytes-set! buffer r gray)
        (bytes-set! buffer g gray)
        (bytes-set! buffer b gray)
        (scale (fx+ r 4))))
    (define gray : Bitmap (make-object bitmap% w h #false #true bs))
    (send gray set-argb-pixels 0 0 w h buffer)
    gray))

(define bitmap-grayscale/lightness : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte
                        (min (quotient (+ (max r g b) (min r g b)) 2) 255)))))

(define bitmap-grayscale/average : (-> Bitmap Bitmap)
  (lambda [bmp]
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte
                        (assert (quotient (+ r g b) 3) byte?)))))

(define bitmap-grayscale/luminosity : (->* (Bitmap) (Nonnegative-Flonum Nonnegative-Flonum Nonnegative-Flonum) Bitmap)
  (lambda [bmp [ro 0.2126] [go 0.7152] [bo 0.0722]]
    (bitmap-grayscale bmp
                      (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte
                        (min (exact-round (+ (* r ro) (* g go) (* b bo))) 255)))))

(define bitmap-grayscale/decomposition : (-> Bitmap (U 'max 'min) Bitmap)
  (lambda [bmp algorithm]
    (bitmap-grayscale bmp
                      (cond [(eq? algorithm 'min) min]
                            [else (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte (assert (max r g b) byte?))]))))

(define bitmap-grayscale/channel : (-> Bitmap (U 'red 'green 'blue) Bitmap)
  (lambda [bmp channel]
    (bitmap-grayscale bmp
                      (case channel
                        [(red) (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte r)]
                        [(green) (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte g)]
                        [else (λ [[r : Byte] [g : Byte] [b : Byte]] : Byte b)]))))
