#lang typed/racket

(provide (all-defined-out))

(require geofun/vector)
(require geofun/digitama/font)

(require "shared.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (for/list : (Listof (Pairof String Geo)) ([face (in-list (list-math-font-faces))])
    (cons face
          (geo-text* "ℎⁱⱼₖₗₘₙ; 𝑥=𝑎+𝑏−𝑐×𝑑÷𝑒; ∫₀∞𝑓(𝑔)𝑒⁻ˣ²𝑑𝑥; √π/2; ∀𝑥∈ℝ, ∃𝑦∈ℂ: |𝑥−𝑦| < ε"
                     (desc-font #:family face)))))
