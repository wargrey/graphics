#lang typed/racket/base

(provide (all-defined-out))

(require digimon/number)
(require math/matrix)

(require "cie.rkt")

(require "digitama/cie.rkt")
(require "digitama/correction.rkt")
(require "digitama/constants.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-make-OKLAB-RGB-convertors : (-> [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [#:rgb-filter [rgb-filter values]]
    (define coRGB CIE-sRGB-D65)
    (define coXYZ (matrix-inverse coRGB))

    (values (λ [L a b]
              (define LMS (matrix* CIE-OKLAB->LMS-D65 (col-matrix [L a b])))
              (define XYZ (matrix* CIE-LMS->XYZ-D65 (matrix-map (λ [[c : Flonum]] (* c c c)) LMS)))
              (define RGB (matrix* coXYZ XYZ))
              (define-values (R G B)
                (rgb-filter (matrix-ref RGB 0 0)
                            (matrix-ref RGB 1 0)
                            (matrix-ref RGB 2 0)))

              (color-gamma-encode R G B))
            
            (λ [R G B]
              (define-values (r g b) (color-gamma-decode R G B))
              (define XYZ (matrix* coRGB (col-matrix [r g b])))
              (define LMS (matrix* CIE-XYZ->LMS-D65 XYZ))
              (define Lab (matrix* CIE-LMS->OKLAB-D65 (matrix-map flcbrt LMS)))
              
              (values (matrix-ref Lab 0 0)
                      (matrix-ref Lab 1 0)
                      (matrix-ref Lab 2 0))))))

(define CIE-make-OKLCH-RGB-convertors : (-> [#:rgb-filter CIE-Filter] (Values CIE<->RGB CIE<->RGB))
  (lambda [#:rgb-filter [rgb-filter values]]
    (define-values (OKLab->RGB RGB->OKLab) (CIE-make-OKLAB-RGB-convertors #:rgb-filter rgb-filter))
    (define epsilon 0.000004)

    (values (λ [L C h]
              (define-values (a b) (CIE-LCh->Lab C h))
              (OKLab->RGB L a b))
            
            (λ [R G B]
              (define-values (L a b) (RGB->OKLab R G B))
              (define-values (chroma hue) (CIE-Lab->LCh a b epsilon))
              (values L chroma hue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-values (oklab->rgb rgb->oklab) (CIE-make-OKLAB-RGB-convertors))
(define-values (oklch->rgb rgb->oklch) (CIE-make-OKLCH-RGB-convertors))
