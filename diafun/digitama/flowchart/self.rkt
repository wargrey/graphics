#lang typed/racket/base

(provide (all-defined-out))

(require geofun/digitama/dc/path)
(require geofun/digitama/dc/composite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct dia:flow geo:group
  ([skeleton : Geo:Path])
  #:type-name Dia:Flow
  #:transparent)
