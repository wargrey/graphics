#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(unsafe-require/typed/provide
 "pangocairo.rkt"
 [pango_version (-> Integer)]
 [cairo_version (-> Integer)]
 
 [pango_version_string (-> String)]
 [cairo_version_string (-> String)])
