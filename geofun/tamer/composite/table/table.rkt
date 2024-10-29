#lang typed/racket

(require geofun/vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define examples : (Listof Geo)
  (list (geo-text "Field Name: ")    (geo-text "Testcase for (bitmap-table)")
        (geo-text "Specification: ") (geo-paragraph "* Shown in a table of 2 rows;\n* This Field has no examples." #:max-width 300)
        (geo-text "Examples: ")))

(define monospace : Font (desc-font #:family 'monospace))
(define geobjs : (Listof Geo)
  (time (build-list 1350 (λ _ (let ([rc (random #xFFFFFF)])
                                (geo-text #:color (rgb* rc)
                                          (string-upcase (format "~x" rc))
                                          monospace))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (geo-table 2 examples '(rc lc) '(ct) '(8) '(8))
  
  (collect-garbage)
  (collect-garbage)
  (collect-garbage) ; 36ms
  (geo-flsize (time (geo-vl-append* geobjs)))
  
  (collect-garbage)
  (collect-garbage)
  (collect-garbage) ; 112ms
  (time (geo-freeze (geo-table 26 geobjs '(rc) '(cc) '(10) '(10)))))
  