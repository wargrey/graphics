#lang typed/racket/base

(require psd/base)
(require psd/profile)
(require psd/bitmap)

(require geofun/vector)

(require racket/format)
(require digimon/debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define HDR.psd (collection-file-path "HDR.psd" "psd" "tamer" "samples"))

(define read-hdr : (-> Flonum Geo)
  (lambda [expose]
    (geo-vc-append #:gapsize 2.0
                   (geo-scale (geo-bitmap (read-psd-bitmap HDR.psd #:hdr-expose expose)) 0.4)
                   (geo-text (format "exposure: ~a" (~r expose #:precision '(= 1)))))))

(define hdr.psd (time* (read-psd HDR.psd)))

(psd-profile hdr.psd)
hdr.psd

(geo-table 4 (time* (map read-hdr (list -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)))
           'cc 'cc 4.0 4.0)
