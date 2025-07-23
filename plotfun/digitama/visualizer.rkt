#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "visualizer/interface.rkt"))
(provide Plot-Visualizer-Tree Plot-Visualizer plot-visualizer?)

(provide Plot:Lines plot:lines? lines)
(provide Plot:Function plot:function? function f:linear)

(require "visualizer/self.rkt")
(require "visualizer/interface.rkt")

(require "visualizer/function.rkt")
(require "visualizer/line.rkt")
