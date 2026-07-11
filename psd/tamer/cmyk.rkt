#lang typed/racket/base

(require psd/base)
(require psd/profile)
(require psd/bitmap)

(require geofun/vector)
(require colorspace/cmyk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CMYK.psd (collection-file-path "cmyk16.psd" "psd" "tamer" "samples"))

(define read-cmyk : (-> CMYK-Ink-Config Geo)
  (lambda [ink]
    (geo-vc-append #:gapsize 2.0
                   (geo-bitmap (read-psd-bitmap CMYK.psd #:cmyk-ink ink))
                   (geo-text (format "CMYK Ink: ~a" ink)))))

(define cmyk.psd (read-psd CMYK.psd))

(psd-profile cmyk.psd)

(geo-table 4 (map read-cmyk (list 'US 'Europe 'Japan))
           'cc 'cc 4.0 4.0)
