#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)
(require racket/string)
(require racket/symbol)
(require racket/pretty)

(require geofun/resize)

(require geofun/digitama/richtext/self)

(require geofun/digitama/dc/more)
(require geofun/digitama/layer/void)
(require geofun/digitama/geometry/anchor)

(require diafun/digitama/flowchart/style)
(require diafun/digitama/block/dc)
(require diafun/digitama/track/style)
(require diafun/digitama/interface)

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
    (gensym (format "^~a" postfix))))

(define plt-flow-terminal->anchor : (-> Any (U Symbol))
  (lambda [prefix]
    (string->symbol (format "~a$" prefix))))

(define plt-flow-function->anchor : (-> Any (U Symbol Keyword))
  (lambda [f]
    (cond [(procedure? f) (assert (object-name f) symbol?)]
          [(or (symbol? f) (keyword? f)) f]
          [(string? f) (string->symbol f)]
          [else (string->symbol (format "~a" f))])))

(define plt-flow-file-anchor : (-> Input-Port Index Keyword)
  (lambda [in amount]
    (define body (peek-string amount 0 in))
    
    (string->keyword
     (cond [(eof-object? body) (string-append "/doc/" pltflow-delim "#<eof>" (symbol->immutable-string (gensym pltflow-delim)))]
           [(eof-object? (peek-byte in amount)) (string-append "/doc/"  body)]
           [else (string-append "/doc/" pltflow-delim body "..." (symbol->immutable-string (gensym pltflow-delim)))]))))

(define #:forall (F) plt-flow-function-rename : (-> (U (Listof (∩ F Procedure)) (Vectorof (∩ F Procedure))) (Listof F))
  (lambda [fs]
    (for/list ([f (if (list? fs) (in-list fs) (in-vector fs))]
               [idx (in-naturals 1)])
      (define desc (format "~a" (object-name f)))
      (define name (string->symbol (format pltflow-delim-format desc idx)))
      
      ((inst procedure-rename F) f name))))

(define plt-flow-block-describe : (-> Geo-Anchor-Name String (U String Void False))
  (lambda [id text]
    (and (> (string-length text) 0)
         (eq? (string-ref text 0)
              (string-ref pltflow-delim 0))
         (let ([ts (string-split text pltflow-delim)])
           (and (pair? ts)
                (car ts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-block-construct : Dia-Anchor->Block
  (lambda [id label style width height direction subtype]
    (cond [(diaflow-process-style? (car style)) (plt-flow-block-process  id label style width height direction subtype)]
          [(diaflow-start-style? (car style))   (plt-flow-block-terminal id label style width height direction subtype)]
          [(diaflow-stop-style? (car style))    (plt-flow-block-terminal id label style width height direction subtype)]
          [(diaflow-storage-style? (car style)) (when (eq? subtype 'File)
                                                  (plt-flow-block-document id label style width height direction subtype))])))

(define plt-flow-arrow-identify : Dia-Track-Identifier
  (lambda [source target labels extra]
    (dia-track-style-construct source target labels (default-diaflow-storage-arrow-style-make) make-diaflow-storage-arrow-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define plt-flow-block-process : Dia-Block-Create
  (lambda [id label style width height direction subtype]
    (define fbox : Geo:Sandglass
      (geo-sandglass #:fill (dia-block-resolve-fill-paint style)
                     #:stroke (dia-block-resolve-stroke-paint style)
                     #:neck-width (* width 0.22) #:neck-height (* width 0.12)
                     (* width 0.25)))

    (if (or (not direction) (not (zero? direction)))
        (create-dia-block #:id id #:type 'Process subtype
                          #:fit-region 1.00 0.36 0.00 0.00
                          #:with fbox label)
        (create-dia-block #:id id #:type 'Process subtype
                          #:fit-region 1.00 0.74 0.0 0.10
                          #:with (geo-rotate fbox (- direction (* pi 0.5))) label))))

(define plt-flow-block-terminal : Dia-Block-Create
  (lambda [id label style width height direction subtype]
    (create-dia-block #:id id #:type 'Storage #false
                      #:with the-void-geo #false)))

(define plt-flow-block-document : Dia-Block-Create
  (lambda [block-key label style width height direction subtype]
    (define hratio : Nonnegative-Flonum 0.85)
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-region 1.0 hratio 0.0 0.0
                      #:alignment 0.0 0.0
                      #:create-with style [geo-document width height `(,(* (- 1.0 hratio) 0.5) :)]
                      label)))
