#lang typed/racket/base

(provide (all-defined-out))

(require digimon/stdio)

(require "exn.rkt")
(require "image/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define select-psd-file : (-> Path-String Positive-Real Boolean (Values Path-String Positive-Real))
  (lambda [src.psd density try?]
    (cond [(not try?) (values src.psd density)]
          [else (let* ([path.psd : String (if (string? src.psd) src.psd (path->string src.psd))]
                       [path@2x.psd : String (regexp-replace #rx"([.][^.]*|)$" path.psd "@2x\\1")])
                  (cond [(not (file-exists? path@2x.psd)) (values path.psd density)]
                        [else (values path@2x.psd (+ density density))]))])))

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

(define skip-psd-subsection : (-> Input-Port Positive-Byte (Option Fixnum))
  (lambda [/dev/psdin psd/psb-size]
    (define _color-mode-data : Void (skip-mn:bytes /dev/psdin 4))
    (define _images-resources : Void (skip-mn:bytes /dev/psdin 4))
    (define layer+mask-size : Index (read-msize /dev/psdin psd/psb-size))

    (and (> layer+mask-size 0)
         (unsafe-fx+ (file-position /dev/psdin) layer+mask-size))))

(define read-psd-composite-image : (-> Input-Port (Option Fixnum) (Values PSD-Compression-Method Bytes))
  (lambda [/dev/psdin image-pos]
    (unless (not image-pos) (file-position /dev/psdin image-pos))
    
    (values (integer->compression-method (read-muint16 /dev/psdin) throw-enum-error)
            (read-rest-bytes /dev/psdin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-psd-subsection : (-> Input-Port Positive-Byte (Values Bytes Bytes (Option Bytes) (Option Bytes) (Option Bytes) (Option Fixnum)))
  (lambda [/dev/psdin psd/psb-size]
    (define color-mode-data : Bytes (read-mn:bytes /dev/psdin 4)) ; for indexed or duotone images
    (define images-resources : Bytes (read-mn:bytes /dev/psdin 4))
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
          
          (values color-mode-data images-resources
                  (and (> layer-size 0) layer-info)
                  (and (> mask-size 0) global-mask-info)
                  (and (> tagged-blocks-size 0) (read-nbytes /dev/psdin tagged-blocks-size))
                  image-pos))
        (values color-mode-data images-resources #false #false #false #false))))
