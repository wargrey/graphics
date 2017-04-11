#lang typed/racket

(provide (all-defined-out))

(require racket/string)
(require racket/bool)

(require colorspace/misc)

(require "digitama/digicore.rkt")
(require "digitama/color.rkt")
(require "digitama/cheat.rkt")
(require "digitama/inspectable.rkt")

(define-type RGBA-Color (Instance RGBA-Color%))

(define-type RGBA-Color%
  (Class #:implements Inspectable-Color%
         (init [red Byte #:optional]
               [green Byte #:optional]
               [blue Byte #:optional]
               [alpha Real #:optional])
         (init-field [immutable? Boolean #:optional])
         (init-rest Null) #|disable string based constructor|#))

(define/make-is-a? rgba% : RGBA-Color%
  (class inspectable-color%
    (init [red (fxmin (random 255) 255)]
          [green (fxmin (random 255) 255)]
          [blue (fxmin (random 255) 255)]
          [alpha 1.0])
    
    (init-field [immutable? #true])
    
    (super-make-object red green blue alpha)

    (define/override (set red green blue [alpha 1.0])
      (when immutable? (error 'rgba% "color is immutable"))
      (super set red green blue alpha))

    (define/override (copy-from src)
      (set (send this red) (send this green) (send this blue) (send this alpha))
      this)
    
    (define/override (is-immutable?)
      immutable?)

    (define/override (inspect)
      (list (send this red) (send this green) (send this blue) (send this alpha)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-color : (->* (Color+sRGB) (Nonnegative-Flonum) RGBA-Color)
  (let ([colorbase : (HashTable Fixnum RGBA-Color) (make-hasheq)])
    (lambda [representation [alpha 1.0]]
      (define opaque? : Boolean (fl= alpha 1.0))
      (cond [(fixnum? representation)
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
            [(not (rgba%? representation))
             (hash-ref! colorbase
                        (eq-hash-code representation)
                        (λ [] (select-color (rgb-bytes->hex (send representation red) (send representation green) (send representation blue))
                                            (flmax (real->double-flonum (send representation alpha)) 0.0))))]
            [else representation]))))
