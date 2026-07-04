#lang typed/racket/base

(provide (all-defined-out))

(require racket/vector)

(require "base.rkt")

(require "digitama/self.rkt")
(require "digitama/layer/self.rkt")
(require "digitama/layer/block.rkt")
(require "digitama/resource/self.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-profile : (->* (PSD) (Output-Port #:prefix String #:full? Boolean) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""] #:full? [full? #true]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (fprintf out "~a~a Object[~a]:~n" prefix (if (psd? self) 'PSD 'PSB) (psd-file-name self))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (psd-header-width self) (psd-header-height self))
    (fprintf out "~aDepth: [~a * ~a]~n" ~t (psd-header-depth self) (psd-header-channels self))
    (fprintf out "~aColor Mode: ~a~n" ~t (psd-header-color-mode self))
    (fprintf out "~aCompression Method: ~a~n" ~t (psd-header-compression-method self))

    (define resources : PSD-Image-Resources (psd-body-resources self))
    (define layers : (Listof PSD-Layer) (psd-layers self))
    (define mask : (Option PSD-Global-Mask-Info) (psd-global-mask self))
    (define blocks : PSD-Layer-Tagged-Blocks (psd-tagged-blocks self))
    
    (fprintf out "~aResources: ~a~n" ~t (hash-keys resources))
    (fprintf out "~aLayer Count: ~a~n" ~t (length layers))

    (cond [(null? layers) (newline out)]
          [else (fprintf out "~aGlobal Layer Mask: ~a~n~aTagged Blocks: ~a~n~n"
                         ~t (if (not mask) 'None (vector-drop (struct->vector mask) 2))
                         ~t (hash-keys blocks))])
    
    (unless (not full?)
      (for ([layer (in-list layers)])
        (psd-layer-profile layer out #:prefix ~t)))))

(define psd-layer-profile : (->* (PSD-Layer) (Output-Port #:prefix String) Void)
  (lambda [self [out (current-output-port)] #:prefix [prefix ""]]
    (define-values (~t ~t~t) (values (string-append prefix "    ") (string-append prefix "        ")))

    (define +/- : Symbol (if (psd-layer-object-has-transparency-data? self) '- '+))
    (define record : PSD-Layer-Record (psd-layer-object-record self))
    (define mask : (Option PSD:Layer:Mask) (psd-layer-record-mask record))
    
    (fprintf out "~aLayer Object[~a]: '~a'~n" prefix (psd-layer-object-id self) (psd-layer-object-name self))
    (fprintf out "~aType: ~a~n" ~t
             (cond [(psd:layer:open? self) "Open Folder"]
                   [(psd:layer:closed? self) "Closed Folder"]
                   [(psd:layer:divider? self) "Folder Boundary"]
                   [else "Normal"]))
    
    (fprintf out "~aLocation: (~a, ~a)~n" ~t (psd-layer-header-x record) (psd-layer-header-y record))
    (fprintf out "~aSize: [~a * ~a]~n" ~t (psd-layer-header-width record) (psd-layer-header-height record))
    (fprintf out "~aChannels: ~a~a~n" ~t +/- (for/list : (Listof Any) ([c (in-list (psd-layer-object-channels self))])
                                               (cons (psd-layer-channel-id c)
                                                     (psd-layer-channel-compress-method c))))
    (fprintf out "~aBlend Mode: ~a~n" ~t (psd-layer-header-blend record))
    (fprintf out "~aOpacity: ~a~n" ~t (psd-layer-header-opacity record))
    (fprintf out "~aClipping: ~a~n" ~t (if (zero? (psd-layer-header-clipping record)) 'base 'nonbase))
    (fprintf out "~aFlags: ~a~n" ~t (psd-layer-header-flags record))
    (fprintf out "~aMask: ~a~n" ~t (or mask 'None))
    (fprintf out "~aAdditional Information: ~a~n~n" ~t (hash-keys (psd-layer-blocks self)))))
