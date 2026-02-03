#lang typed/racket/base

(provide (all-defined-out))

(require geofun/font)
(require geofun/stroke)
(require geofun/digitama/paint/self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-preset-track-label-font : Font (desc-font #:size 'normal #:family 'monospace))
(define dia-preset-block-tag-font : Font (desc-font #:size 'normal #:family 'monospace #:weight 'bold))
(define dia-preset-block-caption-font : Font (desc-font #:size 'xx-large))
(define dia-preset-header-font : Font (desc-font #:size 'large #:weight 'bold))
(define dia-preset-file-font : Font (desc-font #:family 'monospace #:size 'xx-large))

(define dia-preset-block-stroke : Pen (desc-stroke #:width 2.0 #:color 'DarkGrey #:join 'round #:cap 'round))
(define dia-preset-track-stroke : Pen (desc-stroke #:width 2.0 #:color 'DimGrey #:join 'round #:cap 'round))
(define dia-preset-free-track-stroke : Pen (desc-stroke #:width 1.5 #:color 'DimGrey #:dash 'long-dash  #:join 'round #:cap 'round))

(define dia-preset-note-font : Font (desc-font #:family 'monospace #:size 'normal))
(define dia-preset-note-track-stroke : Pen (desc-stroke #:width 1.0 #:color 'Grey #:dash #(3.0 3.0) #:join 'round #:cap 'round))
(define dia-preset-note-block-stroke : Pen (desc-stroke #:width 1.0 #:color 'DimGrey #:join 'round #:cap 'round))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-preset-header-font-tweak : Font:Tweak (make-font:tweak #:size 'large #:weight 'bold))
(define dia-preset-object-font-tweak : Font:Tweak (make-font:tweak #:size 'large))
(define dia-preset-note-tag-font-tweak : Font:Tweak (make-font:tweak #:weight 'bold))
