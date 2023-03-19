#lang typed/racket

(require math/flonum)

(require "../cie.rkt")

(define generate-spectural-locus-for-c++ : (->* (CIE-Observer Symbol Symbol (-> CIE-XYZ-Matching-Curves FlVector)) (Byte) Void)
  (lambda [observer fltype varname curve-value [cols 8]]
    (define curves (CIE-observer->XYZ-matching-curves observer #:λ-span 1.0))
    (define total (CIE-XYZ-matching-curves-count curves))
    (define C (curve-value curves))

    (printf "static const ~a ~a [] = {~n    " fltype varname)

    (for ([v (in-flvector C)]
          [n (in-naturals 1)])
      (printf "~a~a" v (if (eq? fltype 'float) 'F ""))

      (if (< n total)
          (if (= (remainder n cols) 0)
              (printf ",~n    ")
              (display ", "))
          (printf "~n};~n")))))

(module+ main
  (pretty-print-columns 80)
  (current-print pretty-print-handler)

  (define-values (observer illuminants) (CIE-load-default-spectrum-samples))
  (define column : Byte 6)
  
  (generate-spectural-locus-for-c++ observer 'double 'wavelength_xbars CIE-XYZ-matching-curves-X column)
  (newline)
  (generate-spectural-locus-for-c++ observer 'double 'wavelength_ybars CIE-XYZ-matching-curves-Y column)
  (newline)
  (generate-spectural-locus-for-c++ observer 'double 'wavelength_zbars CIE-XYZ-matching-curves-Z column))
