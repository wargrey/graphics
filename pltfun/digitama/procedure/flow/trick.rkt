#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)
(require racket/pretty)

(require geofun/digitama/track/anchor)
(require geofun/digitama/richtext/self)

(require diafun/digitama/block/style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define pltflow-delim : String (string #\rubout))
(define pltflow-delim-format : String (string-append pltflow-delim "~a" pltflow-delim ":~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (In)
  plt-flow-input-desc : (case-> [Any -> String]
                                [In (U Geo-Option-Rich-Text (-> In (U Void Geo-Rich-Text))) -> Geo-Rich-Text])
  (case-lambda
    [(v alt-in)
     (cond [(rich-datum<%>? alt-in) alt-in]
           [(procedure? alt-in) (let ([r (alt-in v)]) (if (void? r) "" r))]
           [(and alt-in) alt-in]
           [else (plt-flow-input-desc v)])]
    [(v)
     (pretty-format #:mode (if (char? v) 'write 'display)
                    (cond [(port? v) (object-name v)]
                          [(void? v) ""]
                          [else v]))]))

(define #:forall (In Out)
  plt-flow-output-desc : (case-> [Any -> Geo-Rich-Text]
                                 [Any (Option (-> Any (U Void Geo-Rich-Text))) -> Geo-Rich-Text]
                                 [(U Geo-Option-Rich-Text (-> Out (U Void Geo-Rich-Text))) (U (-> In Out) Datum) In -> Geo-Rich-Text])
  (case-lambda
    [(alt-out f in)
     (cond [(rich-datum<%>? alt-out) alt-out]
           [(procedure? alt-out) (if (procedure? f) (let ([r (alt-out (f in))]) (if (void? r) "" r)) (plt-flow-output-desc f))]
           [(procedure? f) (if (not alt-out) (plt-flow-output-desc (f in)) alt-out)]
           [else (or alt-out (plt-flow-input-desc f))])]
    [(v f)
     (cond [(not f) (plt-flow-output-desc v)]
           [else (plt-flow-output-desc (f v))])]
    [(v) #;|Yes, reuse the one for input| (plt-flow-input-desc v)]))

(define plt-flow-start->anchor : (-> Any (U Symbol))
  (lambda [postfix]
    (gensym (format ">>~a" postfix))))

(define plt-flow-terminal->anchor : (-> Any (U Symbol))
  (lambda [prefix]
    (string->symbol (format "<<~a" prefix))))

(define plt-flow-file-anchor : (-> Input-Port Index Keyword)
  (lambda [in amount]
    (define body (peek-string amount 0 in))
    
    (string->keyword
     (cond [(eof-object? body) (string-append "/doc/" pltflow-delim "#<eof>" (symbol->immutable-string (gensym pltflow-delim)))]
           [(eof-object? (peek-byte in amount)) (string-append "/doc/" pltflow-delim body (symbol->immutable-string (gensym pltflow-delim)))]
           [else (string-append "/doc/" pltflow-delim body "..." (symbol->immutable-string (gensym pltflow-delim)))]))))

(define plt-flow-function->anchor : (-> Any (U Symbol Keyword))
  (lambda [f] ;; prefixed with the '\' for function names containing special characters
    (cond [(procedure? f) (plt-flow-function->anchor (assert (object-name f) symbol?))]
          [(string? f) (string->symbol (string-append "\\" f))]
          [else (string->symbol (format "\\~a" f))])))

(define #:forall (F) plt-flow-function-rename : (-> (U (Listof (∩ F Procedure)) (Vectorof (∩ F Procedure))) (Listof F))
  (lambda [fs] ;; the delimiter ensure that it would be identified as a process block
    (for/list ([f (if (list? fs) (in-list fs) (in-vector fs))]
               [idx (in-naturals 1)])
      (define desc (format "~a" (object-name f)))
      (define name (string->symbol (format pltflow-delim-format desc idx)))
      
      ((inst procedure-rename F) f name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (S M) plt-flow-block-describe : (-> Geo-Anchor-Name String (Dia-Block-Style-Spec S) M Geo-Maybe-Rich-Text)
  (lambda [id text style metadata]
    (define caption : (U String False Void)
      (cond [(zero? (string-length text)) #false]
            [(eq? (string-ref text 0) (string-ref pltflow-delim 0))
             (let ([ts (string-split text pltflow-delim)])
               (when (pair? ts)
                 (car ts)))]))

    (cond [(not (eq? metadata 'File)) caption]
          [(string? caption) (dia-block-text->caption caption style #:id id #:alignment 'left #:trim? #false)]
          [else caption])))
