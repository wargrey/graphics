#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_71546

(provide (all-defined-out))

(require "../block.rkt")
(require "../../exn.rkt")
(require "../../parser.rkt")
(require "../../unsafe/layer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-tagged-blocks-parse : (case-> [Bytes Positive-Byte Index -> (Values PSD-Layer-Tagged-Blocks Index)]
                                          [Bytes Positive-Byte -> PSD-Layer-Tagged-Blocks])
  (case-lambda
    [(self ps-size start0)
     (let parse-8BIM ([start : Index start0]
                      [blocks : PSD-Layer-Tagged-Blocks (hasheq)])
       (if (regexp-match? #px"^8B(IM|64)" self start)
           (let-values ([(key idx size) (parse-8B64 self (unsafe-idx+ start 4) ps-size)])
             (define block : (Option PSD-Layer-Tagged-Block)
               (parse-tagged-block key /psd/layer/blocks
                                   (λ [[parse : PSD-Layer-Tagged-Block-Parser]] (parse self idx size null))
                                   (λ [] (throw-unsupported-error (current-ioexn-input-port) psd-tagged-blocks-parse
                                                                  "unimplemeneted tagged information: ~a" key))
                                   psd-warn-broken-information))
             (parse-8BIM (unsafe-idx+ idx size)
                         (cond [(not block) blocks]
                               [else (hash-set blocks key block)])))
           (values blocks start)))]
    [(self ps-size)
     (let-values ([(blocks _) (psd-tagged-blocks-parse self ps-size 0)])
       blocks)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-8B64 : (-> Bytes Index Positive-Byte (Values Symbol Index Index))
  (lambda [src start ps-size]
    (define key : Symbol (parse-keyword src start))
    (define size-idx : Index (unsafe-idx+ start 4))
    (define ssize : Byte
      (cond [(= ps-size 4) ps-size]
            [(memq key '(lmsk lr16 lr32 layr mt16 mt32 mtrn alph fmsk lnk2 feid fxid pxsd)) 8]
            [else 4]))
    (define size : Index (parse-size src size-idx ssize))
    (define idx : Index (unsafe-idx+ size-idx ssize))

    (values key idx size)))

(define parse-tagged-block : (-> Symbol Path-String (-> PSD-Layer-Tagged-Block-Parser PSD-Layer-Tagged-Block)
                                 (-> PSD-Layer-Tagged-Block) (-> exn:fail Any) (Option PSD-Layer-Tagged-Block))
  (lambda [key parser-dir do-with-parser fallback on-error]
    (define parser : PSD-Layer-Tagged-Block-Parser
      (hash-ref! psd-layer-info-parsers key
                 (λ [] (let ([~a.rkt (build-path parser-dir (string-append (string-downcase (symbol->string key)) ".rkt"))])
                         (with-handlers ([exn? (λ [[e : exn]] (make-fallback-parser fallback))])
                           (assert (dynamic-require ~a.rkt key) psd-layer-tagged-block-parser?))))))

    (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
      (do-with-parser parser))))
