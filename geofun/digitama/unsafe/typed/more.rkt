#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "c.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 "../cairo.rkt"
 [cairo_version (-> Index)]
 [cairo_version_string (-> String)]

 [cairo_push_group (-> Cairo-Ctx Void)]
 [cairo_pop_group_to_source (-> Cairo-Ctx Void)]

 [cairo_pdf_surface_set_metadata (-> Cairo-Stream-Surface Symbol (Option String) Void)]
 [cairo_status_to_string (-> Fixnum String)])
