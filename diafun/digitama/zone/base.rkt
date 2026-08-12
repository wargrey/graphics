#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [remake-#%dia-zone-style remake-dia-zone-style]))

(require geofun/digitama/misc)

(require/provide "self.rkt")
(require/provide "metadata.rkt")
(require/provide "interface.rkt")

(require/provide "instance/flow.rkt")
(require/provide "instance/uml.rkt")

(require "style.rkt")
