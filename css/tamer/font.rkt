#lang racket

(require bitmap)

(for/list ([face (in-list (get-face-list))])
  (bitmap-frame #:color 'gray
                (bitmap-text #:baseline-color 'green #:ascentline-color 'red
                             (string-append face ": Sphinx") (make-font #:face face #:size 32))))
