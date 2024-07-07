#lang typed/racket/base

(require bitmap/projection)
(require bitmap/composite)
(require bitmap/paint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define R : Flonum 72.0)
(define edge-stroke (desc-stroke #:color 'ghostwhite #:width 1))
(define border-stroke (desc-stroke #:width 2))

(bitmap-cc-superimpose
 (bitmap-icosahedron-side-projection R 'vertex #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'lime) #:fill 'azure)
 (bitmap-icosahedron-side-projection R 'edge   #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'blue))
 (bitmap-icosahedron-side-projection R 'face   #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'red)))

(bitmap-cc-superimpose
 (bitmap-icosahedron-over-projection R 'vertex #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'lime) #:fill 'azure)
 (bitmap-icosahedron-over-projection R 'edge   #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'blue))
 (bitmap-icosahedron-over-projection R 'face   #:edge edge-stroke #:border (desc-stroke border-stroke #:color 'red)))
