#lang typed/racket

(provide (all-defined-out))

(require racket/string)
(require racket/bool)

(require colorspace/misc)

(require "digitama/digicore.rkt")
(require "digitama/color.rkt")
(require "digitama/cheat.rkt")

(define-type Color (Instance RGBA-Color%))

(define-type RGBA-Color%
  (Class #:implements Color%
         (init [red Byte #:optional]
               [green Byte #:optional]
               [blue Byte #:optional]
               [alpha Real #:optional])
         (init-rest Null) ; disable string based constructor
         [get-source (-> FlVector)]))

(define/make-is-a? rgba% : RGBA-Color%
  (class color%
    (init [red (fxmin (random 255) 255)]
          [green (fxmin (random 255) 255)]
          [blue (fxmin (random 255) 255)]
          [alpha 1.0])
    
    (super-make-object red green blue alpha)

    (define flcolor : FlVector
      (flvector (byte->gamut red)
                (byte->gamut green)
                (byte->gamut blue)
                (real->gamut alpha)))
    
    (define/public (get-source) flcolor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-color : (->* (Color+sRGB) (Nonnegative-Flonum) Color)
  (let ([colorbase : (HashTable Fixnum Color) (make-hasheq)])
    (lambda [representation [alpha 1.0]]
      (define opaque? : Boolean (fl= alpha 1.0))
      (cond [(exact-integer? representation)
             (define hashcode : Nonnegative-Fixnum (fxand representation #xFFFFFF))
             (hash-ref! colorbase
                        (if opaque? hashcode (eqv-hash-code (make-rectangular hashcode alpha)))
                        (λ [] (let-values ([(r g b) (hex->rgb-bytes representation)])
                                (make-object rgba% r g b alpha))))]
            [(symbol? representation)
             (let try-again ([color-name : Symbol representation]
                             [downcased? : Boolean #false])
               (cond [(hash-has-key? css-named-colors color-name)
                      (hash-ref! colorbase
                                 (cond [(and opaque?) (eq-hash-code color-name)]
                                       [else (equal-hash-code (cons color-name alpha))])
                                 (λ [] (select-color (hash-ref css-named-colors color-name) alpha)))]
                     [(not downcased?) (try-again (string->symbol (string-downcase (symbol->string color-name))) #true)]
                     [(eq? color-name 'currentcolor) (select-color ((default-make-currentcolor)))]
                     [else (select-color #x000000 (if (eq? color-name 'transparent) 0.0 alpha))]))]
            [(string? representation)
             (let* ([color-name (string-downcase (string-replace representation #px"(?i:grey)" "gray"))]
                    [color (send the-color-database find-color color-name)])
               (cond [(false? color) (select-color #x000000 alpha)]
                     [else (hash-ref! colorbase
                                      (equal-hash-code (if opaque? color-name (cons color-name alpha)))
                                      (λ [] (select-color (rgb-bytes->hex (send color red) (send color green) (send color blue))
                                                          alpha)))]))]
            [(rgba%? representation) representation]
            [else (hash-ref! colorbase
                             (eq-hash-code representation)
                             (λ [] (select-color (rgb-bytes->hex (send representation red) (send representation green) (send representation blue))
                                                 (flmax (real->double-flonum (send representation alpha)) 0.0))))]))))

(define color->source : (-> (Instance Color%) FlVector)
  (lambda [color]
    (cond [(rgba%? color) (send color get-source)]
          [else (flvector (byte->gamut (send color red))
                          (byte->gamut (send color green))
                          (byte->gamut (send color blue))
                          (real->gamut (send color alpha)))])))
