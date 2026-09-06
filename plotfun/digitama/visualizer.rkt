#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "visualizer/interface.rkt"))
(provide Plot-Visualizer-Tree Plot-Visualizer Geo-Visualizer plot-visualizer?)

(provide Plot:Lines plot:lines? lines)
(provide Plot:Function plot:function? function f:linear f:kx+b)
(provide Plot:Scatter plot:scatter? points)
(provide sticker)

(require "visualizer/self.rkt")
(require "visualizer/interface.rkt")

(require "visualizer/plot/function.rkt")
(require "visualizer/plot/scatter.rkt")
(require "visualizer/plot/sticker.rkt")
(require "visualizer/plot/line.rkt")
