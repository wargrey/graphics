#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/digicore.rkt")
(require "color.rkt")
(require "paint.rkt")
(require "font.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(void transparent hilite black)

(define white   : FlRGBA (rgb* 'white))
(define gray    : FlRGBA (rgb* 'gray))
(define brown   : FlRGBA (rgb* 'brown))
(define magenta : FlRGBA (rgb* 'magenta))

(define red     : FlRGBA (rgb* 'red))
(define orange  : FlRGBA (rgb* 'orange))
(define yellow  : FlRGBA (rgb* 'yellow))
(define green   : FlRGBA (rgb* 'green))
(define blue    : FlRGBA (rgb* 'blue))
(define cyan    : FlRGBA (rgb* 'cyan))
(define purple  : FlRGBA (rgb* 'purple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define solid            : Stroke (desc-stroke (default-stroke) #:dash 'solid))
(define dot              : Stroke (desc-stroke (default-stroke) #:dash 'dot))
(define dot-dash         : Stroke (desc-stroke (default-stroke) #:dash 'dot-dash))
(define short-dash       : Stroke (desc-stroke (default-stroke) #:dash 'short-dash))
(define long-dash        : Stroke (desc-stroke (default-stroke) #:dash 'long-dash))

(define solid-frame      : Stroke (desc-stroke (default-border) #:dash 'solid))
(define dot-frame        : Stroke (desc-stroke (default-border) #:dash 'dot))
(define dot-dash-frame   : Stroke (desc-stroke (default-border) #:dash 'dot-dash))
(define short-dash-frame : Stroke (desc-stroke (default-border) #:dash 'short-dash))
(define long-dash-frame  : Stroke (desc-stroke (default-border) #:dash 'long-dash))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sans-serif : Font (desc-font #:family 'sans-serif))
(define serif      : Font (desc-font #:family 'serif))
(define monospace  : Font (desc-font #:family 'monospace))
(define fantasy    : Font (desc-font #:family 'fantasy))
(define cursive    : Font (desc-font #:family 'cursive))
(define system-ui  : Font (desc-font #:family 'system-ui))
(define emoji      : Font (desc-font #:family 'emoji))
(define math       : Font (desc-font #:family 'math))
(define fangsong   : Font (desc-font #:family 'fangsong))
