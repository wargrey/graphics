#lang typed/racket/base

(provide (all-defined-out))
  
(require "writer.rkt")
(require "../typed/cairo.rkt")
(require "../visual/ctype.rkt")

(require racket/file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Cairo-Image-Surface-Writer (-> Any Bytes Index Index))
(define-type Cairo-Image-Stream-Source-Make (-> (Values Bitmap-Surface Boolean)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-image-surface-writer : (-> Output-Port Positive-Index Cairo-Image-Surface-Writer)
  (lambda [/dev/imgout pool-size]
    (define cairo-write (make-cairo-vector-surface-writer /dev/imgout pool-size))
    
    (λ [ignored-closure bstr-ptr len]
      (cairo-write bstr-ptr len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-image-stream-write : (-> (U Path-String Output-Port) Positive-Index Cairo-Image-Stream-Source-Make Void)
  (lambda [/dev/pngout pool-size λsurface]
    (if (output-port? /dev/pngout)
        (let ([png-write (make-cairo-image-surface-writer /dev/pngout pool-size)])
          (start-breakable-atomic)
          (let-values ([(surface destroy?) (λsurface)])
            (cairo_surface_flush surface)
            (cairo_surface_write_to_png_stream surface png-write)
            (when destroy? (cairo_surface_destroy surface))
            (end-breakable-atomic)))
        (let ()
          (make-parent-directory* /dev/pngout)
          (call-with-output-file* /dev/pngout #:exists 'truncate/replace
            (λ [[/dev/pthout : Output-Port]]
              (cairo-image-stream-write /dev/pthout pool-size λsurface)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-cairo-image-stream-bytes : (-> Positive-Index Cairo-Image-Stream-Source-Make Bytes)
  (lambda [pool-size λsurface]
    (define /dev/stdout (open-output-bytes '/dev/pngout))
    
    (cairo-image-stream-write /dev/stdout pool-size λsurface)
    (get-output-bytes /dev/stdout)))

(define open-cairo-input-image-stream : (-> Positive-Index Cairo-Image-Stream-Source-Make Input-Port)
  (lambda [pool-size λsurface]
    (define-values (/dev/stdin /dev/stdout) (make-pipe))

    (define ghostcat
      (thread
       (λ []
         (cairo-image-stream-write /dev/stdout pool-size λsurface)
         (close-output-port /dev/stdout))))
    
    /dev/stdin))
