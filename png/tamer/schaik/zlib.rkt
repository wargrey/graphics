#lang typed/racket/base

(provide png-test-zlib-decompression)

(require "../../base.rkt")
(require typed/racket/draw)

;;; http://www.schaik.com/pngsuite/pngsuite_zlb_png.html

(define zlib-decomporess : (-> Path-String PNG)
  (lambda [src.png]
    (define z??n2c08.png (collection-file-path src.png "png" "tamer" "schaik"))

    (time (read-bitmap z??n2c08.png 'unknown/alpha))
    (time (read-png z??n2c08.png))))


(define png-test-zlib-decompression : (-> (Listof (U PNG String)))
  (lambda []
    (define all-sources : (Listof Path-String) (list "z00n2c08.png" "z00n2c08.png" "z03n2c08.png" "z06n2c08.png" "z09n2c08.png"))
    (for/list ([src.png (in-list all-sources)])
      (with-handlers ([exn? exn-message])
        (zlib-decomporess src.png)))))
