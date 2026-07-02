#lang typed/racket/base

(provide psd-layer-info-parse! psd-infobase-ref)

(require typed/racket/unsafe)

(require "../layer/format.rkt")
(require "../exn.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (define (psd-layer-info-parser? func)
    (and (procedure? func)
         (eq? (procedure-arity func) 4))))

(unsafe-require/typed
 (submod "." unsafe)
 [psd-layer-info-parser? (-> Any Boolean : PSD-Layer-Info-Parser)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-info-parsers : (HashTable Symbol PSD-Layer-Info-Parser) (make-hasheq))

(define psd-layer-info-parse! : (-> PSD-Layer-Infobase Symbol Path-String (-> PSD-Layer-Info-Parser PSD-Layer-Info)
                                    (-> PSD-Layer-Info) (-> exn:fail Any) (Option PSD-Layer-Info))
  (lambda [infobase key parser-dir do-with-parser fallback on-error]
    (define parser : PSD-Layer-Info-Parser
      (hash-ref! psd-layer-info-parsers key
                 (λ [] (let ([~a.rkt (build-path parser-dir (string-append (string-downcase (symbol->string key)) ".rkt"))])
                         (with-handlers ([exn? (λ [[e : exn]] (make-fallback-parser fallback))])
                           (assert (dynamic-require ~a.rkt key) psd-layer-info-parser?))))))
    (define info : (Option PSD-Layer-Info)
      (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
        (do-with-parser parser)))
    (cond [(not info) (hash-remove! infobase key)]
          [else (hash-set! infobase key info)])
    info))

(define make-fallback-parser : (-> (-> PSD-Layer-Info) PSD-Layer-Info-Parser)
  (lambda [fallback]
    (λ [[bs : Bytes] [idx : Fixnum] [size : Index] [args : (Listof Any)]] : PSD-Layer-Info
      (fallback))))

(define psd-infobase-ref : (-> Symbol PSD-Layer-Infobase Symbol (Option PSD-Layer-Info))
  (lambda [src infobase key]
    (define maybe-info : (U PSD-Layer-Info PSD-Layer-Segment Void) (hash-ref infobase key void))
    (or (and (PSD-Layer-Info? maybe-info) maybe-info)
        (and (vector? maybe-info)
             (let-values ([(block start size) (values (vector-ref maybe-info 0) (vector-ref maybe-info 1) (vector-ref maybe-info 2))])
               (psd-layer-info-parse!
                infobase key /psd/layer/blocks
                (λ [[parse : PSD-Layer-Info-Parser]] (parse block start size null))
                (λ [] (throw-unsupported-error (current-ioexn-input-port) src "unimplemeneted tagged information: ~a" key))
                psd-warn-broken-information))))))
