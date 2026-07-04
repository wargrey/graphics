#lang typed/racket/base

;;; https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577409_pgfId-1031423

(provide (all-defined-out))

(require racket/case)

(require "self.rkt")
(require "block.rkt")
(require "block/parser.rkt")

(require "../exn.rkt")
(require "../parser.rkt")
(require "../image/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layers-parse : (-> Bytes Positive-Byte Positive-Flonum (Listof PSD-Layer))
  (lambda [self ps-size density]
    (define count (parse-int16 self 0)) ; shit PSD, the `count` CAN be negative...
    (define-values (chstep rle-ssize) (if (= ps-size 4) (values 6 2) (values 10 4)))
    
    (let parse-layer : (Listof PSD-Layer) ([sdrocer : (Listof PSD-Layer-Record) null]
                                           [tsilhc : (Listof (Listof PSD-Layer-Channel-Info)) null]
                                           [sskcolb : (Listof PSD-Layer-Tagged-Blocks) null]
                                           [rest : Fixnum (unsafe-fxabs count)]
                                           [idx : Index 2]
                                           [image-total : Index 0])
      (if (> rest 0)
          (let-values ([(record slennahc image-size blocks next-idx) (parse-layer-record self idx ps-size chstep)])
            (parse-layer (cons record sdrocer) (cons slennahc tsilhc) (cons blocks sskcolb)
                         (unsafe-fx- rest 1) next-idx (unsafe-idx+ image-total image-size)))
          (let parse-layer-channels ([records : (Listof PSD-Layer-Record) sdrocer]
                                     [chlist : (Listof (Listof PSD-Layer-Channel-Info)) tsilhc]
                                     [blockss : (Listof PSD-Layer-Tagged-Blocks) sskcolb]
                                     [end-idx : Index (unsafe-idx+ idx image-total)]
                                     [layers : (Listof PSD-Layer) null])
            (if (pair? records)
                (let*-values ([(record slennahc blocks) (values (car records) (car chlist) (car blockss))]
                              [(divider-info) (hash-ref blocks 'lsct void)]
                              [(name-info) (hash-ref blocks 'luni void)]
                              [(id-info) (hash-ref blocks 'lyid void)])
                  (define make-psd-layer ; PSD-Layer-Constructor
                    (if (psd:ltb:section:divider? divider-info)
                        (case/eq (psd:ltb:section:divider-type divider-info)
                                 [(1) psd:layer:open]
                                 [(2) psd:layer:closed]
                                 [(3) psd:layer:divider]
                                 [else psd-layer])
                        psd-layer))
                  (define name : String
                    (cond [(psd:ltb:unicode:name? name-info) (psd:ltb:unicode:name-data name-info)]
                          [else (psd-layer-record-name record)]))
                  (define id : (U Index Symbol)
                    (cond [(psd:ltb:id? id-info) (psd:ltb:id-data id-info)]
                          [else (gensym 'psd:layer:)]))
                  (define-values (channels previous-end-idx)
                    (let parse-layer-channel : (Values (Listof PSD-Layer-Channel) Index)
                      ([slennahc : (Listof PSD-Layer-Channel-Info) slennahc]
                       [previous-end-idx : Index end-idx]
                       [channels : (Listof PSD-Layer-Channel) null])
                      (if (pair? slennahc)
                          (let* ([chsize (cdar slennahc)]
                                 [chidx (unsafe-idx- previous-end-idx chsize)]
                                 [cmethod (integer->compression-method (parse-uint16 self chidx) throw-enum-error)]
                                 [channel (psd-layer-channel (caar slennahc) cmethod (unsafe-idx+ chidx 2) (unsafe-idx- chsize 2))])
                            (parse-layer-channel (cdr slennahc) chidx (cons channel channels)))
                          (values channels previous-end-idx))))
                  (parse-layer-channels (cdr records) (cdr chlist) (cdr blockss) previous-end-idx
                                        (cons (make-psd-layer id name channels (< count 0) record blocks #"")
                                              layers)))
                layers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-layer-record : (-> Bytes Index Positive-Byte Positive-Byte
                                 (values PSD-Layer-Record (Listof PSD-Layer-Channel-Info) Index PSD-Layer-Tagged-Blocks Index))
  (lambda [layer-info idx ps-size channel-step]
    (define-values (x y width height) (parse-rectangle layer-info idx))
    (define channel-count : Index (parse-size layer-info (unsafe-fx+ idx 16) 2))
    (define-values (slennahc image-data-size 8BIM-idx)
      (let cons-info : (Values (Listof PSD-Layer-Channel-Info) Index Index)
        ([channel-No. : Index 0]
         [start-idx : Index (unsafe-idx+ idx 18)]
         [image-data-size : Index 0]
         [slennahc : (Listof PSD-Layer-Channel-Info) null])
        (if (< channel-No. channel-count)
            (let ([channel-id (parse-int16 layer-info start-idx)]
                  [channel-size (parse-size layer-info (unsafe-fx+ start-idx 2) ps-size)])
              (cons-info (unsafe-idx+ channel-No. 1)
                         (unsafe-idx+ start-idx channel-step)
                         (unsafe-idx+ channel-size image-data-size)
                         (cons (cons (layer-channel-id-normalize channel-id) channel-size) slennahc)))
            (values slennahc image-data-size start-idx))))
    (define signature : Bytes (parse-nbytes layer-info 8BIM-idx 4))
    (unless (equal? signature #"8BIM")
      (throw-signature-error (current-ioexn-input-port) parse-layer-record "invalid layer record: ~a" signature))
    (define blend : PSD-Blend-Mode (parse-keyword layer-info (unsafe-idx+ 8BIM-idx 4) psd-blend-mode?))
    (define-values (opacity clipping flags)
      (values (parse-uint8 layer-info (unsafe-fx+ 8BIM-idx 8))
              (parse-uint8 layer-info (unsafe-fx+ 8BIM-idx 9))
              (layer-flags->symbols (parse-uint8 layer-info (unsafe-fx+ 8BIM-idx 10)))))
    (define-values (mask range-idx) (parse-layer-mask layer-info (unsafe-idx+ 8BIM-idx 16)))
    (define-values (blending-ranges name-idx) (parse-layer-blending-ranges layer-info range-idx channel-count))
    (define-values (pascal-name 8B64-idx) (parse-pascal-string*n layer-info name-idx 4))
    (define-values (blocks end-idx) (psd-tagged-blocks-parse layer-info ps-size 8B64-idx))
    
    (values (psd-layer-record x y width height blend opacity clipping flags mask blending-ranges pascal-name)
            slennahc image-data-size blocks
            end-idx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define parse-layer-mask : (-> Bytes Index (Values (Option PSD:Layer:Mask) Index))
  (lambda [layer-info idx]
    (define size : Index (parse-size layer-info idx 4))
    
    (values (and (> size 0)
                 (let-values ([(x y width height) (parse-rectangle layer-info (unsafe-fx+ idx 4))]
                              [(defcolor) (parse-uint8 layer-info (unsafe-fx+ idx 20))]
                              [(flags) (parse-uint8 layer-info (unsafe-fx+ idx 21))]
                              [(mask-idx) (unsafe-idx+ idx 22)])
                   (define-values (parameter real-idx)
                     (cond [(bitwise-bit-set? flags 4) (parse-mask-parameter layer-info mask-idx)]
                           [else (values psd-layer-mask-default-parameter mask-idx)]))
                   (if (= (unsafe-fx+ (unsafe-fx+ idx 4) size) (unsafe-fx+ real-idx 2))
                       (psd:layer:mask x y width height defcolor (mask-flags->symbols flags) parameter)
                       (let-values ([(rx ry rwidth rheight) (parse-rectangle layer-info (unsafe-fx+ real-idx 2))])
                         (psd:layer:mask:user x y width height defcolor (mask-flags->symbols flags) parameter
                                              (mask-flags->symbols (parse-uint8 layer-info (unsafe-fx+ real-idx 0)))
                                              (parse-uint8 layer-info (unsafe-fx+ real-idx 1))
                                              rx ry rwidth rheight)))))
            (unsafe-idx+ (unsafe-idx+ idx 4) size))))

(define parse-layer-blending-ranges : (-> Bytes Index Index (Values PSD-Blending-Ranges Index))
  (lambda [layer-info idx count]
    (define size : Index (parse-size layer-info idx 4))
    (values (cons (parse-blending-range layer-info (unsafe-fx+ idx 4))
                  (let parse-channel-ranges : (Listof (Pairof PSD-Blending-Range PSD-Blending-Range))
                    ([rest : Index count]
                     [range-idx : Index (unsafe-idx+ idx 12)]
                     [segnar : (Listof (Pairof PSD-Blending-Range PSD-Blending-Range)) null])
                    (cond [(= rest 0) (reverse segnar)]
                          [else (parse-channel-ranges (unsafe-fx- rest 1) (unsafe-idx+ range-idx 8)
                                                      (cons (parse-blending-range layer-info range-idx) segnar))])))
            (unsafe-idx+ (unsafe-idx+ idx 4) size))))

(define parse-global-mask-info : (-> Bytes PSD-Global-Mask-Info)
  (lambda [mask-info]
    (psd-global-mask-info (parse-int16 mask-info 0)
                          (list (parse-uint16 mask-info 2)
                                (parse-uint16 mask-info 4)
                                (parse-uint16 mask-info 6)
                                (parse-uint16 mask-info 8))
                          (integer->mask-opacity (parse-int16 mask-info 10))
                          (integer->mask-kind (parse-uint8 mask-info 12)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define psd-layer-mask-default-parameter : PSD-Layer-Mask-Parameter (vector-immutable #false #false #false #false))

(define parse-rectangle : (-> Bytes Fixnum (Values Fixnum Fixnum Index Index))
  (lambda [src idx]
    (define-values (top left bottom right)
      (values (parse-int32 src (unsafe-fx+ idx 0))
              (parse-int32 src (unsafe-fx+ idx 4))
              (parse-int32 src (unsafe-fx+ idx 8))
              (parse-int32 src (unsafe-fx+ idx 12))))
    (values left top (assert (unsafe-fx- right left) index?) (assert (unsafe-fx- bottom top) index?))))

(define parse-mask-parameter : (-> Bytes Index (Values PSD-Layer-Mask-Parameter Index))
  (lambda [src idx]
    (define-values (mask i0) (values (parse-uint8 src idx) (unsafe-idx+ idx 1)))
    (define-values (user-mask-density i1) (if (bitwise-bit-set? mask 0) (values (parse-uint8 src i0) (unsafe-idx+ i0 1)) (values #false i0)))
    (define-values (user-mask-feather i2) (if (bitwise-bit-set? mask 1) (values (parse-double src i1) (unsafe-idx+ i1 8)) (values #false i1)))
    (define-values (vector-mask-density i3) (if (bitwise-bit-set? mask 2) (values (parse-uint8 src i2) (unsafe-idx+ i2 1)) (values #false i2)))
    (define-values (vector-mask-feather i4) (if (bitwise-bit-set? mask 3) (values (parse-double src i3) (unsafe-idx+ i3 8)) (values #false i3)))

    (if (or user-mask-density user-mask-feather vector-mask-density vector-mask-feather)
        (values (vector-immutable user-mask-density user-mask-feather vector-mask-density vector-mask-feather) i4)
        (values psd-layer-mask-default-parameter i4))))

(define parse-blending-range : (-> Bytes Fixnum (Pairof PSD-Blending-Range PSD-Blending-Range))
  (lambda [src idx]
    (define source : PSD-Blending-Range
      (vector-immutable (parse-uint8 src (unsafe-fx+ idx 0))
                        (parse-uint8 src (unsafe-fx+ idx 1))
                        (parse-uint8 src (unsafe-fx+ idx 2))
                        (parse-uint8 src (unsafe-fx+ idx 3))))
    (define dest : PSD-Blending-Range
      (vector-immutable (parse-uint8 src (unsafe-fx+ idx 4))
                        (parse-uint8 src (unsafe-fx+ idx 5))
                        (parse-uint8 src (unsafe-fx+ idx 6))
                        (parse-uint8 src (unsafe-fx+ idx 7))))
    (cons source dest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define layer-channel-id-normalize : (-> Fixnum PSD-Layer-Channel-Id)
  (lambda [channel]
    (case/eq channel
      [(-1) 'transparent] ; mask
      [(-2) 'user] ; user mask
      [(-3) 'user+vector] ; user mask and vector mask
      [else (assert channel index?)])))

(define layer-flags->symbols : (-> Byte (Listof Symbol))
  (lambda [flag]
    (filter symbol?
            (list (and (bitwise-bit-set? flag 0) 'transparency-protected)
                  (and (bitwise-bit-set? flag 1) 'invisible)
                  (and (bitwise-bit-set? flag 2) 'obsolete)
                  (and (bitwise-bit-set? flag 3) (bitwise-bit-set? flag 4) 'irrelevant)))))

(define mask-flags->symbols : (-> Byte (Listof Symbol))
  (lambda [flag]
    (filter symbol?
            (list (and (bitwise-bit-set? flag 0) 'relative)
                  (and (bitwise-bit-set? flag 1) 'disabled)
                  (and (bitwise-bit-set? flag 2) 'inverse)
                  (and (bitwise-bit-set? flag 3) 'actual)
                  (and (bitwise-bit-set? flag 4) 'parameterized)))))
