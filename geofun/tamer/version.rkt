#lang typed/racket/base

(provide (all-defined-out))

(require geofun/version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(printf "Pango Version: ~a~n" (pango-version-string))
(printf "Cairo Version: ~a~n" (cairo-version-string))
