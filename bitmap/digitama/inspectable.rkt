#lang typed/racket

(provide (all-defined-out))
(provide inspectable-font% inspectable-color%
         inspectable-pen% inspectable-brush%)

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
    [(_ src immutable? strerr ([method args ...] ...))
     #'(begin (define/override (method args ...)
                (when immutable? (error src strerr))
                (super method args ...))
              ...)]))

(define-type Inspectable<%>
  (Class [inspect (-> (Listof Any))]
         [custom-print (-> Output-Port Natural Void)]
         [custom-write (-> Output-Port Void)]
         [custom-display (-> Output-Port Void)]))

(define-type Fixed-Pen%
  (Class #:implements/inits Pen%
         [get-style (-> Pen-Style)] #|the official version forgets this method|#))

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
  [Inspectable-Pen%   #:+ Fixed-Pen%]
  [Inspectable-Brush% #:+ Brush%])

(module cheat racket/base
  (provide (all-defined-out))

  (require racket/class)
  (require racket/draw)

  (define inspectable-mixin%
    (lambda [%]
      (class* % (printable<%>) (super-make-object)
        (define/public (inspect) this)
      
        (define/public (custom-print /dev/stdout quote-depth)
          (custom-write /dev/stdout))
      
        (define/public (custom-write /dev/stdout)
          (write (cons (object-name this) (inspect)) /dev/stdout))
      
        (define/public (custom-display /dev/stdout)
          (display (cons (object-name this) (inspect)) /dev/stdout)))))

  (define inspectable-font% (inspectable-mixin% font%))
  (define inspectable-color% (inspectable-mixin% color%))
  (define inspectable-pen% (inspectable-mixin% pen%))
  (define inspectable-brush% (inspectable-mixin% brush%)))

(unsafe-require/typed
 (submod "." cheat)
 [inspectable-font%  Inspectable-Font%]
 [inspectable-color% Inspectable-Color%]
 [inspectable-pen%   Inspectable-Pen%]
 [inspectable-brush% Inspectable-Brush%])
