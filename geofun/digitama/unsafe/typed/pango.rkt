#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(require "../visual/ctype.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unsafe-require/typed/provide
 "../pango.rkt"
 [pango_version (-> Integer)]
 [pango_version_string (-> String)])
