#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 "pangocairo.rkt"
 [pango_version (-> Integer)]
 [cairo_version (-> Integer)]
 
 [pango_version_string (-> String)]
 [cairo_version_string (-> String)]

 [cairo_pdf_surface_set_metadata (-> Cairo-Stream-Surface Symbol (Option String) Void)]
 [cairo_status_to_string (-> Fixnum String)])
