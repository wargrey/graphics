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
(require "../flowchart/interface.rkt")

(require "../node/dc.rkt")
(require "../edge/style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflowlet-blank : Geo:Blank (geo-blank))
(define diaflow-delim : String (string #\rubout))
(define diaflow-delim-format : String (string-append diaflow-delim "~a" diaflow-delim ":~a"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (In) diaflowlet-input-desc : (case-> [Any -> String]
                                                      [In (U False DC-Markup-Text (-> In (U Void DC-Markup-Text))) -> DC-Markup-Text])
  (case-lambda
    [(v alt-in)
     (cond [(procedure? alt-in) (let ([r (alt-in v)]) (if (void? r) #"" r))]
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
     (cond [(procedure? alt-out) (if (procedure? f) (let ([r (alt-out (f in))]) (if (void? r) #"" r)) (diaflowlet-output-desc f))]
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
     (cond [(eof-object? body) (string-append "/doc/" diaflow-delim "<eof>" (symbol->immutable-string (gensym diaflow-delim)))]
           [(eof-object? (peek-byte in amount)) (string-append "/doc/"  body)]
           [else (string-append "/doc/" diaflow-delim body "..." (symbol->immutable-string (gensym diaflow-delim)))]))))

(define #:forall (F) diaflowlet-funcion-rename : (->* ((U (Listof (∩ F Procedure)) (Vectorof (∩ F Procedure)))) (String)
                                                      (Values (Listof F) (Immutable-HashTable Symbol String)))
  (lambda [fs [fmt "~a:~a"]]
   (define-values (func-seman func-anchors)
     (for/fold ([names : (Listof F) null]
                [anchors : (Immutable-HashTable Symbol String) (hasheq)])
               ([f (if (list? fs) (in-list fs) (in-vector fs))]
                [idx (in-naturals 1)])
       (define desc (format "~a" (object-name f)))
       (define name (string->symbol (format fmt desc idx)))
       
       (values (cons ((inst procedure-rename F) f name) names)
               (hash-set anchors name desc))))

    (values (reverse func-seman) func-anchors)))

(define diaflowlet-node-label-string : (-> Geo-Anchor-Name String (U String Void False))
  (lambda [id text]
    (and (> (string-length text) 0)
         (eq? (string-ref text 0) #\rubout)
         (let ([ts (string-split text diaflow-delim)])
           (and (pair? ts)
                (car ts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflowlet-node-construct : DiaFlow-Id->Node-Shape
  (lambda [id label style width height direction hint]
    (case/eq (object-name style)
             [(diaflow-process-style) (diaflowlet-block-process  id label style width height direction hint)]
             [(diaflow-start-style)   (diaflowlet-block-terminal id label style width height direction hint)]
             [(diaflow-stop-style)    (diaflowlet-block-terminal id label style width height direction hint)])))

(define diaflowlet-arrow-identify : DiaFlow-Arrow-Identifier
  (lambda [source target labels]
    (dia-edge-style-construct source target labels (default-diaflow-storage-arrow-style-make) make-diaflow-storage-arrow-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define diaflowlet-block-process : DiaFlow-Block-Create
  (lambda [id label style width height direction hint]
    (define fbox : Geo:Sandglass
      (geo-sandglass #:fill (dia-node-select-fill-paint style)
                     #:stroke (dia-node-select-stroke-paint style)
                     #:neck-width (* width 0.16) #:neck-height (* width 0.10)
                     (* width 0.25)))
    
    (if (or (not direction) (not (zero? direction)))
        (create-dia-node #:id id #:type 'Process hint #:fit-ratio 1.00 0.36 #:position 0.50 0.20 fbox label)
        (create-dia-node #:id id #:type 'Process hint #:fit-ratio 1.00 0.64 #:position 0.50 0.48 (geo-rotate fbox (- direction (* pi 0.5))) label))))

(define diaflowlet-block-terminal : DiaFlow-Block-Create
  (lambda [id label style width height direction hint]
    (create-dia-node #:id id #:type 'Storage #false
                     diaflowlet-blank #false)))
