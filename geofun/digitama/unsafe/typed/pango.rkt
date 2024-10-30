#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 "../pango.rkt"
 [pango_version (-> Index)]
 [pango_version_string (-> String)])
