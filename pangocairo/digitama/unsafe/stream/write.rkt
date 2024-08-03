#lang racket/base

(provide (all-defined-out))
(provide (all-from-out "../pangocairo.rkt"))
  
(require "../pangocairo.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-vector-surface-writer
  (lambda [/dev/vecout pool-size]
    (define pool (make-bytes pool-size))

    (λ [bstr-ptr len]
      (let cairo-write ([src-rst len]
                        [ptr-off 0])
        (define size (unsafe-fxmin src-rst pool-size))
        (define rest-- (unsafe-fx- src-rst size))
        
        (memcpy pool 0 bstr-ptr ptr-off size)
        (write-bytes pool /dev/vecout 0 size)
        
        (if (unsafe-fx> rest-- 0)
            (cairo-write rest-- (unsafe-fx+ ptr-off size))
            CAIRO_STATUS_SUCCESS)))))
  
(define make-cairo-image-surface-writer
  (lambda [/dev/imgout pool-size]
    (define cairo-write (make-cairo-vector-surface-writer /dev/imgout pool-size))
    
    (λ [ignored-closure bstr-ptr len]
      (cairo-write bstr-ptr len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-vector-stream-write
  (lambda [/dev/vecout λsurface flwidth flheight pool-size λmake]
    (if (output-port? /dev/vecout)
        (let* ([surface (λsurface /dev/vecout flwidth flheight pool-size)]
               [cr (cairo_create surface)])
          (start-breakable-atomic)
          (λmake cr flwidth flheight)
          
          (cairo_destroy cr)
          (cairo_surface_flush surface)
          (cairo_surface_destroy surface)
          (end-breakable-atomic))
        (let ()
          (make-parent-directory* /dev/vecout)
          (call-with-output-file* /dev/vecout #:exists 'truncate/replace
            (λ [/dev/pthout]
              (cairo-vector-stream-write /dev/pthout λsurface
                                         flwidth flheight pool-size λmake)))))))

(define cairo-image-stream-write
  (lambda [/dev/pngout pool-size λsurface]
    (if (output-port? /dev/pngout)
        (let-values ([(png-write) (make-cairo-image-surface-writer /dev/pngout pool-size)]
                     [(surface destroy?) (dynamic-wind start-breakable-atomic λsurface end-breakable-atomic)])
          (cairo_surface_flush surface)
          (cairo_surface_write_to_png_stream surface png-write)
          (when destroy? (cairo_surface_destroy surface)))
        (let ()
          (make-parent-directory* /dev/pngout)
          (call-with-output-file* /dev/pngout #:exists 'truncate/replace
            (λ [/dev/pthout]
              (cairo-vector-stream-write /dev/pthout pool-size λsurface)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-vector-stream-bytes
  (lambda [λsurface flwidth flheight pool-size λmake]
    (define /dev/stdout (open-output-bytes '/dev/crout))
    
    (cairo-vector-stream-write /dev/stdout λsurface flwidth flheight pool-size λmake)
    (get-output-bytes /dev/stdout)))

(define open-cairo-input-vector-stream
  (lambda [λsurface flwidth flheight pool-size λmake]
    (define-values (/dev/stdin /dev/stdout) (make-pipe #false '/dev/vecin '/dev/crout))

    (define ghostcat
      (thread
       (λ []
         (cairo-vector-stream-write /dev/stdout λsurface flwidth flheight pool-size λmake)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))

(define make-cairo-image-stream-bytes
  (lambda [pool-size λsurface]
    (define /dev/stdout (open-output-bytes '/dev/crout))
    
    (cairo-image-stream-write /dev/stdout pool-size λsurface)
    (get-output-bytes /dev/stdout)))

(define open-cairo-input-image-stream
  (lambda [pool-size λsurface]
    (define-values (/dev/stdin /dev/stdout) (make-pipe #false '/dev/pngin '/dev/crout))

    (define ghostcat
      (thread
       (λ []
         (cairo-image-stream-write /dev/stdout pool-size λsurface)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))
