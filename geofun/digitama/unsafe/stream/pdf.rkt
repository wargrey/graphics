#lang racket/base

(provide (all-defined-out))

(require racket/date)

(require "write.rkt")
(require "../../parameter/pdf.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define default-pdf-stream-pool-size 4096)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-create-pdf-stream-surface
  (lambda [/dev/pdfout flwidth flheight pool-size]
    (define pdf-write (make-cairo-vector-surface-writer /dev/pdfout pool-size))
    (define surface (cairo_pdf_surface_create_for_stream pdf-write flwidth flheight))
    
    (let ([status (cairo_surface_status surface)])
      (unless (unsafe-fx= status CAIRO_STATUS_SUCCESS)
        (raise-arguments-error 'cairo-create-pdf-stream-surface (cairo_status_to_string status)
                               "width" flwidth "height" flheight "pool size" pool-size)))

    (cairo-set-pdf-metadata surface 'title (default-pdf-title))
    (cairo-set-pdf-metadata surface 'author (default-pdf-author))
    (cairo-set-pdf-metadata surface 'subject (default-pdf-subject))
    (cairo-set-pdf-metadata surface 'keywords (default-pdf-keywords))
    (cairo-set-pdf-metadata surface 'producer (default-pdf-producer))

    (parameterize ([date-display-format 'iso-8601])
      #;(cairo-set-pdf-metadata surface 'ctime (date->string (seconds->date (cairo-path-create-seconds /dev/pdfout) #true) #true))
      (cairo-set-pdf-metadata surface 'mtime (date->string (seconds->date (current-seconds) #true) #true)))
    
    surface))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-pdf-stream-write
  (lambda [/dev/pdfout flwidth flheight λmake]
    (cairo-vector-stream-write /dev/pdfout cairo-create-pdf-stream-surface
                               flwidth flheight default-pdf-stream-pool-size λmake)))

(define make-cairo-pdf-stream-bytes
  (lambda [flwidth flheight λmake]
    (make-cairo-vector-stream-bytes cairo-create-pdf-stream-surface
                                    flwidth flheight default-pdf-stream-pool-size λmake)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define cairo-set-pdf-metadata
  (lambda [surface key value]
    (when (string? value)
      (cairo_pdf_surface_set_metadata surface key value))))

(define cairo-path-create-seconds
  (lambda [/dev/pdfout]
    (cond [(output-port? /dev/pdfout) (cairo-path-create-seconds (object-name /dev/pdfout))]
          [(or (path? /dev/pdfout) (string? /dev/pdfout))
           (if (file-exists? /dev/pdfout)
               (hash-ref (file-or-directory-stat /dev/pdfout) 'creation-time-seconds)
               (current-seconds))]
          [else (current-seconds)])))
