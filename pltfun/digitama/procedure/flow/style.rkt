#lang typed/racket/base

(provide (all-defined-out))

(require digimon/measure)
(require digimon/struct)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)

(require geofun/digitama/path/tip/self)
(require geofun/digitama/path/tip/arrow)

(require diafun/digitama/presets)
(require diafun/digitama/flowchart/style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-preset-track-font : Font (desc-font #:family 'math #:size 'x-large))
(define plt-flow-preset-block-font : Font (desc-font #:family 'math #:size 'xx-large))
(define plt-flow-preset-track-stroke : Pen (desc-stroke dia-preset-track-stroke #:color 'DodgerBlue #:dash 'dot))
(define plt-flow-preset-block-stroke : Pen (desc-stroke dia-preset-block-stroke #:width (&% 75)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration plt-flow-track-backstop-style : Plt-Flow-Track-Backstop-Style #:as dia-track-backstop-style
  #:format "default-plt-flow-track-~a"
  ([font : Font plt-flow-preset-track-font]
   [font-paint : Fill-Paint 'DodgerBlue]
   [line-paint : Stroke-Paint plt-flow-preset-track-stroke]
   [source-tip : Option-Geo-Tip #false]
   [target-tip : Option-Geo-Tip default-arrow-tip]
   [label-rotate? : Boolean #true]
   [label-inline? : Boolean #false]
   [label-distance : (Option Flonum) #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-configuration plt-flow-block-backstop-style : Plt-Flow-Block-Backstop-Style #:as dia-block-backstop-style
  #:format "default-plt-flow-block-~a"
  ([width : Nonnegative-Flonum 150.0]
   [height : Nonnegative-Flonum 75.0] ; 1/2 of the width
   [padding : Dia-Block-Padding (&% 2)]
   [font : Font dia-preset-header-font]
   [font-paint : Fill-Paint 'Black]
   [stroke-paint : Option-Stroke-Paint plt-flow-preset-block-stroke]
   [fill-paint : Option-Fill-Paint 'GhostWhite]))
