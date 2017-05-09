#lang typed/racket

(provide (all-defined-out))
(provide inspectable-font% inspectable-color%
         inspectable-pen%  inspectable-brush%
         inspectable-linear-gradient%
         inspectable-radial-gradient%)

(require typed/racket/unsafe)
(require typed/racket/draw)

(define-syntax (define-inspectable-class stx)
  (syntax-case stx []
    [(_ [Inspect% #:+ Origin%] ...)
     #'(begin (define-inspectable-class Inspect% #:+ Origin%) ...)]
    [(_ Inspect% #:+ Origin% inits ...)
     #'(define-type Inspect%
         (Class #:implements Inspectable<%>
                #:implements/inits Origin%
                inits ...))]))

(define-syntax (define/override-immutable stx)
  (syntax-case stx []
    [(_ src strerr ([method args ...] ...))
     #'(begin (define/override (is-immutable?) #true)
              (define/override (method args ...) (error src strerr))
              ...)]))

(define-type Inspectable<%>
  (Class [inspect (-> (Listof Any))]
         [custom-print (-> Output-Port Natural Void)]
         [custom-write (-> Output-Port Void)]
         [custom-display (-> Output-Port Void)]))

(define-inspectable-class Inspectable-Font% #:+ Font%
  ;;; NOTE
  ;; These named initial arguments cannot be used with (new),
  ;; they just serve as an elegant form of (init-rest).
  (init [size Real]
        [face (Option String)] ; the official version forgets that it can be False
        [family Font-Family]
        [style Font-Style #:optional]
        [weight Font-Weight #:optional]
        [underline? Any #:optional]
        [smoothing Font-Smoothing #:optional]
        [size-in-pixels? Any #:optional]
        [hinting Font-Hinting #:optional]))

(define-inspectable-class Inspectable-Color% #:+ Color%
  (init-rest (U (List String)
                (List Byte Byte Byte)
                (List Byte Byte Byte Real))))

(define-inspectable-class
  [Inspectable-Pen%             #:+ Pen%]
  [Inspectable-Brush%           #:+ Brush%]
  [Inspectable-Linear-Gradient% #:+ Linear-Gradient%]
  [Inspectable-Radial-Gradient% #:+ Radial-Gradient%])

(module cheat racket/base
  (provide (all-defined-out))

  (require racket/class)
  (require racket/draw)

  (define inspectable-mixin%
    (lambda [%]
      (class* % (printable<%>) (super-make-object)
        (define &this (box #false))
        
        (define/public (inspect) this)
      
        (define/public (custom-print /dev/stdout quote-depth)
          (custom-write /dev/stdout))
      
        (define/public (custom-write /dev/stdout)
          (write (self) /dev/stdout))
      
        (define/public (custom-display /dev/stdout)
          (display (self) /dev/stdout))

        (define/private (self)
          (or (unbox &this)
              (let ([self (cons (object-name this) (inspect))])
                (set-box! &this self)
                self))))))

  (define inspectable-font% (inspectable-mixin% font%))
  (define inspectable-color% (inspectable-mixin% color%))
  (define inspectable-pen% (inspectable-mixin% pen%))
  (define inspectable-brush% (inspectable-mixin% brush%))
  (define inspectable-linear-gradient% (inspectable-mixin% linear-gradient%))
  (define inspectable-radial-gradient% (inspectable-mixin% radial-gradient%)))

(unsafe-require/typed
 (submod "." cheat)
 [inspectable-font%            Inspectable-Font%]
 [inspectable-color%           Inspectable-Color%]
 [inspectable-pen%             Inspectable-Pen%]
 [inspectable-brush%           Inspectable-Brush%]
 [inspectable-linear-gradient% Inspectable-Linear-Gradient%]
 [inspectable-radial-gradient% Inspectable-Radial-Gradient%])
