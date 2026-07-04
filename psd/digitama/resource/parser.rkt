#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_38034

(provide (all-defined-out))

(require racket/case)

(require "self.rkt")
(require "format.rkt")

(require "../exn.rkt")
(require "../parser.rkt")
(require "../unsafe/resource.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-image-resources-parse : (-> Bytes Positive-Flonum PSD-Image-Resources)
  (lambda [block density]
    (let parse-8BIM : PSD-Image-Resources ([start : Index 0]
                                           [resources : PSD-Image-Resources (hasheq)])
      (if (regexp-match? #px"^8BIM" block start)
          (let*-values ([(id) (parse-int16 block (unsafe-fx+ start 4))]
                        [(name size-idx) (parse-pascal-string*n block (unsafe-idx+ start 6) 2)]
                        [(segsize) (parse-size block size-idx 4)]
                        [(segstart) (unsafe-idx+ size-idx 4)])
            (define maybe-resource
              (parse-resource id /psd/res
                              (λ [[parse : PSD-Resource-Parser]]
                                (case/eq id
                                  [(#x040C) (parse id name block start segsize (list density))]
                                  [(#x03EF) (parse id name block start segsize (list (hash-has-key? resources #x0435)))]
                                  [else (parse id name block start segsize null)]))
                              (λ [] (throw-unsupported-error (current-ioexn-input-port) psd-image-resources-parse
                                                             "unimplemeneted resource: ~a" (psd-id->string id)))
                              psd-warn-broken-resource))
            
            (parse-8BIM (unsafe-idx+ (unsafe-idx+ segstart segsize)
                                     (unsafe-fxremainder segsize 2))
                        (cond [(not maybe-resource) resources]
                              [else (hash-set resources id maybe-resource)])))
          resources))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-resource : (-> Fixnum Path-String (-> PSD-Resource-Parser PSD-Resource)
                             (-> PSD-Resource) (-> exn:fail Any) (Option PSD-Resource))
  (lambda [id parser-dir do-with-parser fallback on-error]
    (define parser : PSD-Resource-Parser
      (hash-ref! psd-res-parsers id
                 (λ [] (let ([id~a.rkt (build-path parser-dir (format "id~a.rkt" id))]
                             [0xFFFD (string->symbol (format "0x~x" id))])
                         (with-handlers ([exn? (λ [[e : exn]] (make-fallback-parser fallback))])
                           (assert (dynamic-require id~a.rkt 0xFFFD) psd-resource-parser?))))))

    (with-handlers ([exn:fail? (λ [[ef : exn:fail]] (and (on-error ef) #false))])
      (do-with-parser parser))))
