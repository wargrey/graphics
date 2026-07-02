#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)

(require "base.rkt")
(require "layer.rkt")

(require "digitama/self.rkt")
(require "digitama/resource.rkt")
(require "digitama/layer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-profile : (->* (PSD) (Output-Port #:prefix String #:full? Boolean #:resolve? Boolean) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""] #:full? [full? #true] #:resolve? [resolve? #true]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (fprintf out "~a~a Object[~a]:~n" prefix (if (psd? self) 'PSD 'PSB) (psd-file-name self))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (psd-header-width self) (psd-header-height self))
    (fprintf out "~aDepth: [~a * ~a]~n" ~t (psd-header-depth self) (psd-header-channels self))
    (fprintf out "~aColor Mode: ~a~n" ~t (psd-header-color-mode self))
    (fprintf out "~aCompression Method: ~a~n" ~t (psd-section-compression-method self))

    (define resources : PSD-Image-Resources (psd-image-resources self #:resolve? resolve?))
    (define layers : (Listof PSD-Layer-Object) (psd-layers self))
    (define mask : (Option PSD-Global-Layer-Mask) (psd-global-layer-mask self))
    (define infobase : PSD-Layer-Infobase (psd-tagged-blocks self #:resolve? resolve?))
    
    (fprintf out "~aResources: ~a~n" ~t (hash-keys resources))
    (fprintf out "~aLayer Count: ~a~n" ~t (length layers))

    (cond [(null? layers) (newline out)]
          [else (fprintf out "~aGlobal Layer Mask: ~a~n~aTagged Blocks: ~a~n~n"
                         ~t (if (not mask) 'None (vector-drop (struct->vector mask) 2))
                         ~t (hash-keys infobase))])
    
    (unless (not full?)
      (for ([layer (in-list layers)])
        (psd-layer-profile layer out #:prefix ~t #:resolve? resolve?)))))

(define psd-layer-profile : (->* (PSD-Layer-Object) (Output-Port #:prefix String #:resolve? Boolean) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""] #:resolve? [resolve? #true]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (define +/- : Symbol (if (PSD-Layer-Subject-has-transparency-data? self) '- '+))
    (define record : PSD-Layer-Record (PSD-Layer-Subject-record self))
    (define mask : (Option PSD-Layer-Mask) (PSD-Layer-Record-mask record))
    
    (fprintf out "~aLayer Object[~a]: '~a'~n" prefix (PSD-Layer-Subject-id self) (PSD-Layer-Subject-name self))
    (fprintf out "~aType: ~a~n" ~t (cond [(PSD-Layer:Open? self) "Open Folder"]
                                         [(PSD-Layer:Closed? self) "Closed Folder"]
                                         [(PSD-Layer:Divider? self) "Folder Boundary"]
                                         [else "Normal"]))

    (unless (not resolve?) (psd-layer-resolve-infobase self))
    (fprintf out "~aLocation: (~a, ~a)~n" ~t (PSD-Layer-Header-x record) (PSD-Layer-Header-y record))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (PSD-Layer-Header-width record) (PSD-Layer-Header-height record))
    (fprintf out "~aChannels: ~a~a~n" ~t +/- ((inst map Integer PSD-Layer-Channel) car (PSD-Layer-Subject-channels self)))
    (fprintf out "~aBlend Mode: ~a~n" ~t (PSD-Layer-Header-blend record))
    (fprintf out "~aOpacity: ~a~n" ~t (PSD-Layer-Header-opacity record))
    (fprintf out "~aClipping: ~a~n" ~t (if (zero? (PSD-Layer-Header-clipping record)) 'base 'nonbase))
    (fprintf out "~aFlags: ~a~n" ~t (PSD-Layer-Header-flags record))
    (fprintf out "~aMask: ~a~n" ~t (or mask 'None))
    (fprintf out "~aAdditional Information: ~a~n~n" ~t (hash-keys (PSD-Layer-Object-infobase self)))))
