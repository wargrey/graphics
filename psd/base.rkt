#lang typed/racket/base

;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml

(provide (all-defined-out))
(provide (rename-out [bitmap-intrinsic-size psd-size]
                     [bitmap-intrinsic-width psd-header-width]
                     [bitmap-intrinsic-height psd-header-height]
                     [bitmap-density psd-file-density]
                     [bitmap-source psd-file-name]))

(require bitmap/stdio)

(require "digitama/self.rkt")
(require "digitama/image.rkt")
(require "digitama/pixels.rkt")
(require "digitama/stdin.rkt")
(require "digitama/parser.rkt")
(require "digitama/exn.rkt")

(require "digitama/resource.rkt")
(require "digitama/resources/format.rkt")
(require "digitama/unsafe/resource.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct psd psd-section () #:type-name PSD)
(struct psb psd () #:type-name PSB)

(define-read-bitmap psd #:-> PSD
  (lambda [/dev/psdin density]
    (define-values (ps-size channels height width depth color-mode) (read-psd-header /dev/psdin))
    (define-values (color-mode-data image-resources layer-info mask-info tagged-blocks image-pos) (read-psd-subsection /dev/psdin ps-size))
    (define-values (compression-method image-data) (read-psd-composite-image /dev/psdin image-pos))
    
    (create-bitmap (if (= ps-size 4) PSD PSB) /dev/psdin density width height
                   channels depth color-mode color-mode-data image-resources
                   (or layer-info null) mask-info (or tagged-blocks (ann (make-hasheq) PSD-Layer-Infobase))
                   compression-method ps-size
                   (psd-image-decoder read-psd image-data
                                      width height color-mode channels depth
                                      compression-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-depth : (-> PSD (Values Positive-Byte Positive-Byte))
  (lambda [self]
    (values (psd-header-depth self)
            (psd-header-channels self))))

(define psd-color-mode : (-> PSD PSD-Color-Mode)
  (lambda [self]
    (psd-header-color-mode self)))

(define psd-thumbnail-bitmap : (-> PSD False)
  (lambda [self]
    (define maybe-preview : (Option PSD-Thumbnail) (psd-thumbnail self))
    (and maybe-preview (PSD-Thumbnail-image maybe-preview))

    #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_38034
(define psd-image-resources : (-> PSD [#:resolve? (U (Listof Integer) Integer Boolean)] PSD-Image-Resources)
  (lambda [self #:resolve? [ids #true]]
    (define resources : PSD-Image-Resources
      (psd-ref! self section-resources
                (λ [resource-data]
                  (let parse-8BIM : PSD-Image-Resources ([start : Fixnum 0]
                                                         [blocks : (Listof (Pairof Fixnum PSD-Image-Resource-Segment)) null])
                    (if (regexp-match? #px"^8BIM" resource-data start)
                        (let ([id (parse-int16 resource-data (unsafe-fx+ start 4))])
                          (define-values (pascal size-idx) (parse-pascal-string*n resource-data (unsafe-fx+ start 6) 2))
                          (define segsize : Index (parse-size resource-data size-idx 4))
                          (define segstart : Fixnum (unsafe-fx+ size-idx 4))
                          (define block : PSD-Image-Resource-Segment (vector pascal resource-data segstart segsize))
                          (parse-8BIM (unsafe-fx+ (unsafe-fx+ segstart segsize) (unsafe-fxremainder segsize 2)) (cons (cons id block) blocks)))
                        (make-hasheq blocks))))))
    (unless (not ids)
      (for ([id (cond [(list? ids) (in-list ids)]
                      [(integer? ids) (in-value ids)]
                      [else (in-list (hash-keys resources))])])
        (define maybe-res : (U PSD-Image-Resource-Segment PSD-Resource Void) (hash-ref resources id void))
        (when (vector? maybe-res)
          (define-values (name start size) (values (vector-ref maybe-res 0) (vector-ref maybe-res 2) (vector-ref maybe-res 3)))
          (define block : Bytes (vector-ref maybe-res 1))
          (psd-resource-parse!
           resources id /psd/resources
           (λ [[parse : PSD-Resource-Parser]]
             (case id
               [(#x040C) (parse id name block start size (list (bitmap-density self)))]
               [(#x03EF) (parse id name block start size (list (hash-has-key? resources #x0435)))]
               [else (parse id name block start size null)]))
           (λ [] (throw-unsupported-error #false 'psd-resource-ref "unimplemeneted resource: ~a" (psd-id->string id)))
           psd-warn-broken-resource))))
    resources))

(define #:forall (T) psd-resource-ref : (case-> [PSD Integer -> (Option PSD-Resource)]
                                                [PSD Integer (-> Any Boolean : #:+ T) -> (Option T)])
  (case-lambda
    [(self id)
     (let* ([resources : PSD-Image-Resources (psd-image-resources self #:resolve? id)]
            [maybe-res : (U PSD-Image-Resource-Segment PSD-Resource Void) (hash-ref resources id void)])
       (and (PSD-Resource? maybe-res) maybe-res))]
    [(self id pred?)
     (let ([res (psd-resource-ref self id)])
       (and res (pred? res) res))]))

(define psd-resolve-resources : (->* (PSD) ((U (Listof Integer) Integer)) Void)
  (lambda [self [ids #true]]
    (void (psd-image-resources self #:resolve? ids))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-grid+guides : (-> PSD (Option PSD-Grid+Guides)) (λ [self] (psd-resource-ref self #x408 PSD-Grid+Guides?)))
(define psd-thumbnail : (-> PSD (Option PSD-Thumbnail)) (λ [self] (psd-resource-ref self #x40C PSD-Thumbnail?)))
(define psd-file-info : (-> PSD (Option PSD-File-Info)) (λ [self] (psd-resource-ref self #x424 PSD-File-Info?)))
