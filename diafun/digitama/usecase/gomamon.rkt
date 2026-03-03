#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [gomamon-actor-associate! gomamon-actor-use!]
                     [gomamon-associate! gomamon-use!]))

(require geofun/track)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-actor-associate! : (case-> [Gomamon Symbol -> Void]
                                           [Gomamon Complex Symbol -> Void]
                                           [Gomamon Real Real Symbol -> Void])
  (case-lambda
    [(goma target) (gomamon-move-to! goma target)]
    [(goma target case-anchor) (gomamon-move-to! goma target case-anchor)]
    [(goma length radians case-anchor) (gomamon-radial-move! goma length radians case-anchor)]))

(define gomamon-actor-generalize! : (case-> [Gomamon Keyword -> Void]
                                            [Gomamon Complex Keyword -> Void]
                                            [Gomamon Real Real Keyword -> Void])
  (case-lambda
    [(goma target) (gomamon-move-to! goma target)]
    [(goma target actor-anchor) (gomamon-move-to! goma target actor-anchor)]
    [(goma length radians actor-anchor) (gomamon-radial-move! goma length radians actor-anchor)]))

(define gomamon-case-include! : (case-> [Gomamon Symbol -> Void]
                                        [Gomamon Complex Symbol -> Void]
                                        [Gomamon Real Real Symbol -> Void])
  (case-lambda
    [(goma target) (gomamon-move-to! goma target '#:include)]
    [(goma target case-anchor) (gomamon-move-to! goma target case-anchor '#:include)]
    [(goma length radians case-anchor) (gomamon-radial-move! goma length radians case-anchor '#:include)]))

(define gomamon-case-extend! : (case-> [Gomamon Symbol -> Void]
                                       [Gomamon Complex Symbol -> Void]
                                       [Gomamon Real Real Symbol -> Void])
  (case-lambda
    [(goma target) (gomamon-move-back! goma target '#:extend)]
    [(goma target case-anchor) (gomamon-move-back! goma target case-anchor '#:extend)]
    [(goma length radians case-anchor) (gomamon-radial-back! goma length radians case-anchor '#:extend)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define gomamon-associate! : (-> Gomamon Keyword Symbol Void)
  (lambda [goma source target]
    (gomamon-jump-to! goma source)
    (gomamon-actor-associate! goma target)))

(define gomamon-generalize! : (-> Gomamon Keyword Keyword Void)
  (lambda [goma source target]
    (gomamon-jump-to! goma source)
    (gomamon-actor-generalize! goma target)))

(define gomamon-include! : (-> Gomamon Symbol Symbol Void)
  (lambda [goma source target]
    (gomamon-jump-to! goma source)
    (gomamon-case-include! goma target)))

(define gomamon-extend! : (-> Gomamon Symbol Symbol Void)
  (lambda [goma source target]
    (gomamon-jump-to! goma source)
    (gomamon-case-extend! goma target)))
