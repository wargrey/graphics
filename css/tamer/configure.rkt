#lang typed/racket/gui

(provide (all-defined-out))

(require bitmap/digitama/digicore)
(require bitmap/font)

(require "../syntax.rkt")

(define-syntax (time-run stx)
  (syntax-case stx []
    [(_ sexp ...)
     #'(let ([momery0 : Natural (current-memory-use)])
         (define-values (result cpu real gc) (time-apply (thunk sexp ...) null))
         (define momery : Integer (- (current-memory-use) momery0))
         (printf "memory: ~a cpu time: ~a real time: ~a gc time: ~a~n" (~size momery 'Bytes) cpu real gc)
         (car result))]))

(define DrRacket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))

(define value-inspect : (-> Any Any)
  (lambda [raw] ; TODO: deal with cyclics
    (cond [(css-stylesheet? raw) raw]
          [(css-wide-keyword? raw) (css-wide-keyword-datum raw)]
          [(and (struct? raw) (not (object? raw))) (vector-map value-inspect (struct->vector raw))]
          [(vector? raw) (for/vector : (Vectorof Any) ([val (in-vector raw)]) (value-inspect val))]
          [(hash? raw) (for/list : (Listof Any) ([(k v) (in-hash raw)]) (cons (value-inspect k) (value-inspect v)))]
          [(list? raw) (map value-inspect raw)]
          [(pair? raw) (cons (value-inspect (car raw)) (value-inspect (cdr raw)))]
          [(box? raw) (box (value-inspect (unbox raw)))]
          [(color%? raw) (list 'rgba (send raw red) (send raw green) (send raw blue) (send raw alpha))]
          [(css-font%? raw) (value-inspect (send raw inspect))]
          [else raw])))

(define css-configure-@media : (-> Void)
  (lambda []
    (define-values (width height) (get-display-size))
    (define DrPrint : (-> Any AnyValues) (current-print))
    (css-deprecate-media-type #true)
    (default-css-media-type 'screen)
    (current-print (λ [[raw : Any]] (DrPrint (value-inspect raw))))
    (default-css-media-features
      (make-css-media-features #:width (real->double-flonum (or width 0))
                               #:height (real->double-flonum (or height 0))
                               #:resolution (real->double-flonum (or (get-display-backing-scale) 1.0))
                               #:update 'fast
                               #:overflow-block 'scroll
                               #:overflow-inline 'scroll
                               #:color (get-display-depth)
                               #:pointer 'fine #:any-pointer 'fine
                               #:hover 'hover #:any-hover 'hover))))

(define units : (Listof Symbol) '(KB MB GB TB))
(define ~size : (->* (Real) (Symbol #:precision (U Integer (List '= Integer))) String)
  (lambda [size [unit 'Bytes] #:precision [prcs '(= 3)]]
    (if (eq? unit 'Bytes)
        (cond [(< -1024.0 size 1024.0) (~a (exact-round size) "Bytes")]
              [else (~size (fl/ (real->double-flonum size) 1024.0) 'KB #:precision prcs)])
        (let try-next-unit : String ([s : Flonum (real->double-flonum size)] [us : (Option (Listof Symbol)) (memq unit units)])
          (cond [(false? us) "Typed Racket is buggy if you see this message"]
                [(or (fl< (flabs s) 1024.0) (null? (cdr us))) (string-append (~r s #:precision prcs) (symbol->string (car us)))]
                [else (try-next-unit (fl/ s 1024.0) (cdr us))])))))
