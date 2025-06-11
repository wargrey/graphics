#lang typed/racket/base

(require geofun/vector)
(require colorspace/ok)

(require "misc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define size 64)

(define rgbs
  (list (rgb 0.4906 0.1387 0.1590)
        (rgb 0.7761 0.3634 0.0245)
        (rgb 0.6165 0.5751 0.0928)
        (rgb 0.4073 0.6512 0.2235)
        (rgb 0.3829 0.6727 0.9385)
        (rgb 0.5020 0.0000 0.5020)))

(geo-table*
 (list (cons (geo-text 'RGB)
             (for/list : (Listof Geo) ([rgb (in-list rgbs)])
               (geo-solid rgb size)))
               
       (list (geo-text 'Lab)
             (geo-solid (lab 0.292345       39.3825     20.0664) size)
             (geo-solid (lab 0.522345       40.1645     59.9971) size)
             (geo-solid (lab 0.602345      -5.36540     58.9560) size)
             (geo-solid (lab 0.622345      -34.9638     47.7721) size)
             (geo-solid (lab 0.675345      -8.69110    -41.6019) size)
             (geo-solid (lab 0.296900  44888/100000 -2904/10000) size))
               
       (list (geo-text 'LCh)
             (geo-solid (lch 0.292345         44.2 27.00) size)
             (geo-solid (lch 0.522345         72.2 56.20) size)
             (geo-solid (lch 0.602345         59.2 95.20) size)
             (geo-solid (lch 0.622345         59.2 126.2) size)
             (geo-solid (lch 0.675345         42.5 258.2) size)
             (geo-solid (lch 0.296900 45553/100000 327.1) size))
       
       (list (geo-text 'OKLab)
             (geo-solid (oklab 0.40101  0.1147  0.0453) size)
             (geo-solid (oklab 0.59686  0.1009  0.1192) size)
             (geo-solid (oklab 0.65125 -0.0320  0.1274) size)
             (geo-solid (oklab 0.66016 -0.1084  0.1114) size)
             (geo-solid (oklab 0.72322 -0.0465 -0.1150) size)
             (geo-solid (oklab 0.42100  41/100 -25/100) size))
       
       (list (geo-text 'OKLCh)
             (geo-solid (oklch 0.40101    0.12332  21.5550) size)
             (geo-solid (oklch 0.59686    0.15619  49.7694) size)
             (geo-solid (oklch 0.65125    0.13138 104.0970) size)
             (geo-solid (oklch 0.66016    0.15546 134.2310) size)
             (geo-solid (oklch 0.72322    0.12403 247.9960) size)
             (geo-solid (oklch 0.42100 4825/10000 328.4000) size)))
 'rc 'cc 16.0 16.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define geo-display-hcl : (-> Nonnegative-Real Geo)
  (lambda [light]
    (geo-ht-append #:gapsize 16.0
                   (geo-text (format "light: ~a%" (* light 100)))
                 
                   (geo-table*
                    (for/list : (Listof (Listof Geo)) ([chroma (in-range 101)])
                      (for/list : (Listof Geo) ([hue (in-range 1 360)])
                        (geo-rectangle 1 2 #:stroke #false #:fill (lch light (/ chroma 100) hue))))
                    'rc 'cc)
                   
                   (geo-table*
                    (for/list : (Listof (Listof Geo)) ([chroma (in-range 101)])
                      (for/list : (Listof Geo) ([hue (in-range 1 360)])
                        (geo-rectangle 1 2 #:stroke #false #:fill (oklch light (/ chroma 100) hue))))
                    'rc 'cc))))

'HSV
(geo-table*
 (for/list : (Listof (Listof Geo)) ([chroma (in-range 0.0 101.0 1.0)])
   (for/list : (Listof Geo) ([hue (in-range 360)])
     (geo-rectangle 1 2 #:stroke #false #:fill (hsv hue (* chroma 0.01) 1.0))))
 'rc 'cc)

(geo-display-hcl 0.1)
(geo-display-hcl 0.2)
(geo-display-hcl 0.3)
(geo-display-hcl 0.4)
(geo-display-hcl 0.5)
(geo-display-hcl 0.6)
(geo-display-hcl 0.7)
(geo-display-hcl 0.8)
(geo-display-hcl 0.9)
(geo-display-hcl 1.0)
