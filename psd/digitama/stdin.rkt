#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

(require "exn.rkt")
(require "image/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-psd-header : (-> Input-Port (Values Positive-Byte Positive-Byte Positive-Index Positive-Index Positive-Byte PSD-Color-Mode))
  (lambda [/dev/psdin]
    (define signature : Bytes (read-nbytes /dev/psdin 4))
    (define version : Integer (read-muint16 /dev/psdin))
    
    (unless (and (equal? signature #"8BPS")
                 (or (= version 1)
                     (= version 2)))
      (raise-user-error 'read-psd-header "this is not a valid PSD/PSB file: ~a" (object-name /dev/psdin)))
    
    (skip-nbytes /dev/psdin 6) ; reserved

    (values (if (= version 1) 4 8)
            (assert (read-muint16 /dev/psdin) positive-byte?)   ; channels
            (assert (read-muint32 /dev/psdin) positive-index?)  ; height
            (assert (read-muint32 /dev/psdin) positive-index?)  ; width
            (assert (read-muint16 /dev/psdin) positive-byte?)   ; depth
            (integer->color-mode (read-muint16 /dev/psdin) throw-enum-error))))

(define read-psd-image-resources : (-> Input-Port (Values Bytes Bytes))
  (lambda [/dev/psdin]
    (values (read-mn:bytes /dev/psdin 4) ; for indexed or duotone images
            (read-mn:bytes /dev/psdin 4))))

(define skip-psd-layer-section : (-> Input-Port Positive-Byte (Option Fixnum))
  (lambda [/dev/psdin psd/psb-size]
    (define layer+mask-size : Index (read-msize /dev/psdin psd/psb-size))

    (and (> layer+mask-size 0)
         (unsafe-fx+ (file-position /dev/psdin) layer+mask-size))))

(define read-psd-composite-image : (-> Input-Port (Option Fixnum) (Values PSD-Compression-Method Bytes))
  (lambda [/dev/psdin image-pos]
    (unless (not image-pos) (file-position /dev/psdin image-pos))
    
    (values (integer->compression-method (read-muint16 /dev/psdin) throw-enum-error)
            (read-rest-bytes /dev/psdin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-psd-layer-section : (-> Input-Port Positive-Byte (Values (Option Bytes) (Option Bytes) (Option Bytes) (Option Fixnum)))
  (lambda [/dev/psdin psd/psb-size]
    (define layer+mask-size : Index (read-msize /dev/psdin psd/psb-size))

    (if (> layer+mask-size 0)
        (let ([image-pos (unsafe-fx+ (file-position /dev/psdin) layer+mask-size)])
          (define layer-info : Bytes (read-mn:bytes /dev/psdin psd/psb-size))
          (define global-mask-info : Bytes (read-mn:bytes /dev/psdin 4))
          (define layer-size : Index (bytes-length layer-info))
          (define mask-size : Index (bytes-length global-mask-info))

          ;;; WARNING:
          ; Software like GIMP may add some garbage bytes somewhere,
          ;   so, the file pointer is more reliable than calculated position.
          (define tagged-blocks-size : Fixnum (unsafe-fx- image-pos (file-position /dev/psdin)))
          
          (values (and (> layer-size 0) layer-info)
                  (and (> mask-size 0) global-mask-info)
                  (and (> tagged-blocks-size 0) (read-nbytes /dev/psdin tagged-blocks-size))
                  image-pos))
        (values #false #false #false #false))))
