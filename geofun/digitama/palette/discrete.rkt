#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [the-tab10-palette the-classic-palette])
         (rename-out [the-pastel1-palette the-macaron-palette])
         (rename-out [the-dark2-palette the-vintage-palette]))

(require digimon/measure)

(require "base.rkt")
(require "../../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define list-palette-create
  (lambda [#:stroke-scale [scale : Real 0.75] #:name [name : (Option Symbol) #false]
           [colors : (Pairof Color (Listof Color))]] : Palette-Index->Colors
    (define color-db : (Vectorof (Pairof FlRGBA FlRGBA))
      (for/vector : (Vectorof (Pairof FlRGBA FlRGBA)) ([c (in-list colors)])
        (define fill-color (rgb* c))
        (define stroke-color
          (cond [(>= scale 0.0) (rgb* (oklch-modulate fill-color #:lightness scale))]
                [else fill-color]))
        
        (cons stroke-color fill-color)))

    (procedure-rename
     (λ [[idx : Natural] [bg : (Option Color) #false]]
       (vector-ref color-db (remainder idx (vector-length color-db))))
     (string->symbol (format "~a/~a" (or name (gensym 'palette:list)) (vector-length color-db))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Tableau family
(define the-tab10-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:tab10
                       (list #x1f77b4 #xff7f0e #x2ca02c #xd62728 #x9467bd
                             #x8c564b #xe377c2 #x7f7f7f #xbcbd22 #x17becf)))

(define the-tab20-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:tab20
                       (list #x1f77b4 #xaec7e8 #xff7f0e #xffbb78 #x2ca02c #x98df8a #xd62728 #xff9896
                             #x9467bd #xc5b0d5 #x8c564b #xc49c94 #xe377c2 #xf7b6d2 #x7f7f7f #xc7c7c7
                             #xbcbd22 #xdbdb8d #x17becf #x9edae5)))

(define the-tab10n-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:tab10n
                       (list #x4e79a7 #xf28e2b #xe15759 #x76b7b2 #x59a14f
                             #xedc948 #xb07aa1 #xff9da7 #x9c755f #xbab0ac)))

(define the-tab20b-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:tab20b
                       (list #x393b79 #x5254a3 #x6b6ecf #x9c9ede #x637939 #x8ca252 #xb5cf6b #xcedb9c
                             #x8c6d31 #xbd9e39 #xe7ba52 #xe7cb94 #x843c39 #xad494a #xd6616b #xe7969c
                             #x7b4173 #xa55194 #xce6dbd #xde9ed6)))

(define the-tab20c-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:tab20c
                       (list #x3182bd #x6baed6 #x9ecae1 #xc6dbef #xe6550d #xfd8d3c #xfdae6b #xfdd0a2
                             #x31a354 #x74c476 #xa1d99b #xc7e9c0 #x756bb1 #x9e9ac8 #xbcbddc #xdadaeb
                             #x636363 #x969696 #xbdbdbd #xd9d9d9)))

;; The ColorBrewer family
(define the-set1-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:set1
                       (list #xe41a1c #x377eb8 #x4daf4a #x984ea3 #xff7f00
                             #xffff33 #xa65628 #xf781bf #x999999)))

(define the-set2-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:set2
                       (list #x66c2a5 #xfc8d62 #x8da0cb #xe78ac3
                             #xa6d854 #xffd92f #xe5c494 #xb3b3b3)))

(define the-set3-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:set3
   (list #x8dd3c7 #xffffb3 #xbebada #xfb8072 #x80b1d3 #xfdb462
         #xb3de69 #xfccde5 #xd9d9d9 #xbc80bd #xccebc5 #xffed6f)))

(define the-dark2-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:dark2
                       (list #x1b9e77 #xd95f02 #x7570b3 #xe7298a
                             #x66a61e #xe6ab02 #xa6761d #x666666)))

(define the-paired-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:paired
                       (list #xa6cee3 #x1f78b4 #xb2df8a #x33a02c #xfb9a99 #xe31a1c
                             #xfdbf6f #xff7f00 #xcab2d6 #x6a3d9a #xffff99 #xb15928)))

(define the-pastel1-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:pastel1
                       (list #xfbb4ae #xb3cde3 #xccebc5 #xdecbe4 #xfed9a6
                             #xffffcc #xe5d8bd #xfddaec #xf2f2f2)))

(define the-pastel2-palette : Palette-Index->Colors
  (list-palette-create #:name 'palette:pastel2
                       (list #xb3e2cd #xfdcdac #xcbd5e8 #xf4cae4
                             #xe6f5c9 #xfff2ae #xf1e2cc #xcccccc)))
