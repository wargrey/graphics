#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/draw)

(define-type Color+sRGB (U String Symbol Integer (Instance Color%)))
(define-type Bitmap (Instance Bitmap%))

(define-type Brush+Color (U Color+sRGB (Pairof Color+sRGB Brush-Style) (Instance Brush%)))
(define-type Pen+Color (U Color+sRGB (Pairof Color+sRGB Pen-Style) (Pairof Color+sRGB Nonnegative-Real)
                          (Pairof Color+sRGB (Pairof Nonnegative-Real Pen-Style)) (Instance Pen%)))
