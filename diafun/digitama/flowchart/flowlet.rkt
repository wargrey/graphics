#lang typed/racket/base

(provide (all-defined-out))

(require racket/case)
(require racket/math)
(require racket/string)
(require racket/symbol)
(require racket/pretty)

(require geofun/digitama/geometry/anchor)
(require geofun/digitama/dc/more)
(require geofun/digitama/dc/plain)
(require geofun/digitama/markup)
(require geofun/resize)

(require "../flowchart/style.rkt")
(require "../block/dc.rkt")
(require "../track/style.rkt")
(require "../interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflow-delim : String (string #\rubout))
(define diaflow-delim-format : String (string-append diaflow-delim "~a" diaflow-delim ":~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (In)
  diaflowlet-input-desc : (case-> [Any -> String]
                                  [In (U False DC-Markup-Text (-> In (U Void DC-Markup-Text))) -> DC-Markup-Text])
  (case-lambda
    [(v alt-in)
     (cond [(pexpr-element? alt-in) alt-in]
           [(procedure? alt-in) (let ([r (alt-in v)]) (if (void? r) #"" r))]
           [(and alt-in) alt-in]
           [else (diaflowlet-input-desc v)])]
    [(v)
     (pretty-format #:mode (if (char? v) 'write 'display)
                    (cond [(port? v) (object-name v)]
                          [(void? v) #""]
                          [else v]))]))

(define #:forall (In Out)
  diaflowlet-output-desc : (case-> [Any -> DC-Markup-Text]
                                   [Any (Option (-> Any (U Void DC-Markup-Text))) -> DC-Markup-Text]
                                   [(U False DC-Markup-Text (-> Out (U Void DC-Markup-Text))) (U (-> In Out) Datum) In -> DC-Markup-Text])
  (case-lambda
    [(alt-out f in)
     (cond [(pexpr-element? alt-out) alt-out]
           [(procedure? alt-out) (if (procedure? f) (let ([r (alt-out (f in))]) (if (void? r) #"" r)) (diaflowlet-output-desc f))]
           [(procedure? f) (if (not alt-out) (diaflowlet-output-desc (f in)) alt-out)]
           [else (or alt-out (diaflowlet-input-desc f))])]
    [(v f)
     (cond [(not f) (diaflowlet-output-desc v)]
           [else (diaflowlet-output-desc (f v))])]
    [(v) #;|Yes, reuse the one for input| (diaflowlet-input-desc v)]))


(define diaflowlet-start->anchor : (-> Any (U Symbol))
  (lambda [postfix]
    (gensym (format "^~a" postfix))))

(define diaflowlet-terminal->anchor : (-> Any (U Symbol))
  (lambda [prefix]
    (string->symbol (format "~a$" prefix))))

(define diaflowlet-function->anchor : (-> Any (U Symbol Keyword))
  (lambda [f]
    (cond [(procedure? f) (assert (object-name f) symbol?)]
          [(or (symbol? f) (keyword? f)) f]
          [(string? f) (string->symbol f)]
          [else (string->symbol (format "~a" f))])))

(define diaflowlet-file-anchor : (-> Input-Port Index Keyword)
  (lambda [in amount]
    (define body (peek-string amount 0 in))
    
    (string->keyword
     (cond [(eof-object? body) (string-append "/doc/" diaflow-delim "#<eof>" (symbol->immutable-string (gensym diaflow-delim)))]
           [(eof-object? (peek-byte in amount)) (string-append "/doc/"  body)]
           [else (string-append "/doc/" diaflow-delim body "..." (symbol->immutable-string (gensym diaflow-delim)))]))))

(define #:forall (F) diaflowlet-function-rename : (-> (U (Listof (∩ F Procedure)) (Vectorof (∩ F Procedure))) (Listof F))
  (lambda [fs]
    (for/list ([f (if (list? fs) (in-list fs) (in-vector fs))]
               [idx (in-naturals 1)])
      (define desc (format "~a" (object-name f)))
      (define name (string->symbol (format diaflow-delim-format desc idx)))
      
      ((inst procedure-rename F) f name))))

(define diaflowlet-block-describe : (-> Geo-Anchor-Name String (U String Void False))
  (lambda [id text]
    (and (> (string-length text) 0)
         (eq? (string-ref text 0) (string-ref diaflow-delim 0))
         (let ([ts (string-split text diaflow-delim)])
           (and (pair? ts)
                (car ts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflowlet-block-construct : Dia-Anchor->Block
  (lambda [id brief style width height direction subtype]
    (case/eq (object-name style)
             [(diaflow-process-style) (diaflowlet-block-process  id brief style width height direction subtype)]
             [(diaflow-start-style)   (diaflowlet-block-terminal id brief style width height direction subtype)]
             [(diaflow-stop-style)    (diaflowlet-block-terminal id brief style width height direction subtype)]
             [(diaflow-storage-style) (when (eq? subtype 'File)
                                        (diaflowlet-block-document id brief style width height direction subtype))])))

(define diaflowlet-arrow-identify : Dia-Track-Identifier
  (lambda [source target labels extra]
    (dia-track-style-construct source target labels (default-diaflow-storage-arrow-style-make) make-diaflow-storage-arrow-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflowlet-block-process : Dia-Block-Create
  (lambda [id brief style width height direction subtype]
    (define fbox : Geo:Sandglass
      (geo-sandglass #:fill (dia-block-resolve-fill-paint style)
                     #:stroke (dia-block-resolve-stroke-paint style)
                     #:neck-width (* width 0.22) #:neck-height (* width 0.12)
                     (* width 0.25)))
    
    (if (or (not direction) (not (zero? direction)))
        (create-dia-block #:id id #:type 'Process subtype #:fit-ratio 1.00 0.36 #:position 0.50 0.20 fbox brief)
        (create-dia-block #:id id #:type 'Process subtype #:fit-ratio 1.00 0.64 #:position 0.50 0.48 (geo-rotate fbox (- direction (* pi 0.5))) brief))))

(define diaflowlet-block-terminal : Dia-Block-Create
  (lambda [id brief style width height direction subtype]
    (create-dia-block #:id id #:type 'Storage #false
                      (geo-blank) #false)))

(define diaflowlet-block-document : Dia-Block-Create
  (lambda [block-key brief style width height direction subtype]
    (define hratio : Nonnegative-Flonum 0.85)
    (define xpos : Nonnegative-Flonum (max (* (/ (default-dia-block-margin) width)  0.5) 0.0))
    (define ypos : Nonnegative-Flonum (max (* (/ (default-dia-block-margin) height) 0.5) 0.0))
    (create-dia-block #:id block-key #:type 'Storage subtype
                      #:fit-ratio 1.0 hratio
                      #:position xpos ypos 0.0 0.0
                      (geo-document #:id (dia-block-shape-id block-key)
                                    #:stroke (dia-block-resolve-stroke-paint style)
                                    #:fill (dia-block-resolve-fill-paint style)
                                    width height `(,(* (- 1.0 hratio) 0.5) :))
                      brief)))
