#lang typed/racket

(require "../constructor.rkt")

(define desc : String "Hello,\aRacket!")

(bitmap-frame (bitmap-desc desc 34) #:color 'Teal)
(bitmap-frame (bitmap-desc desc 34 #:max-height 16) #:color 'Teal)
(bitmap-frame (bitmap-desc desc -1 #:max-height -1) #:color 'Teal)
