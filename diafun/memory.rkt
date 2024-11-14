#lang typed/racket/base

(provide (all-defined-out))

(require racket/sequence)
(require racket/keyword)

(require digimon/format)
(require digimon/metrics)

(require geofun/font)
(require geofun/paint)
(require geofun/stroke)
(require geofun/composite)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/dc/rect)
(require geofun/digitama/dc/text)
(require geofun/digitama/dc/arrow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Memory-Variable (U Keyword Symbol))
(define-type Dia-Memory-State-Procedure (-> (-> Dia-Memory-Variable Byte Void) (-> Dia-Memory-Variable Byte) (->* () (Symbol) Void) Any))
(define-type Dia-Memory-State-Snapshot (->* (Bytes (Listof Dia-Memory-Variable)) (Index) Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshot
  (lambda [#:id [id : (Option Symbol) #false] #:gapsize [gapsize : Real 8.0] #:reverse? [reverse? : Boolean #false]
           #:address-offset [address0 : Index #x20000] #:value-base [vbase : Byte 10]
           #:font [font : Font (desc-font #:family 'monospace)] #:grid-width [width : Real -1.0] #:grid-height [height : Real -2.8]
           #:memory-stroke [stroke : Maybe-Stroke-Paint 'Black] #:memory-fill [fill : Maybe-Fill-Paint 'GhostWhite]
           #:variable-color [vcolor : Color 'ForestGreen] #:variable-fill [vfill : (Option Color) #false]
           #:temp-color [tcolor : Color 'DodgerBlue] #:temp-fill [tfill : (Option Color) #false]
           #:raw-color [rcolor : Color 'DimGray] #:raw-fill [rfill : (Option Color) 'LightGray]
           #:ignored-color [igr-color : Color 'LightGray]
           [memory : Bytes] [variables : (Listof Dia-Memory-Variable)] [body-idx : Index 0]] : Geo
    (define-values (byte-width byte-height) (text-size "0000000000" font))
    (define larger-font : Font (desc-font font #:size 'larger))
    (define grid-width  (max (~length width  byte-width)  byte-width))
    (define grid-height (max (~length height byte-height) (* byte-height 2.0)))
    (define memory-idx : Index (bytes-length memory))

    (define memory-units : (Listof (Listof (Option Geo)))
      (let gen-row : (Listof (Listof (Option Geo))) ([idx : Nonnegative-Fixnum 0]
                                                     [vars : (Listof Dia-Memory-Variable) variables]
                                                     [swor : (Listof (Listof (Option Geo))) null])
        (cond [(>= idx memory-idx) (if reverse? swor (reverse swor))]
              [else (let-values ([(self rest) (cond [(< idx body-idx) (values #false vars)]
                                                    [(pair? vars) (values (car vars) (cdr vars))]
                                                    [else (values #false null)])])
                      (define address-desc : String (string-append "0x" (~hexstring (+ address0 idx))))
                      (define datum : Byte (bytes-ref memory idx))
                      (define binary-desc : String (~binstring datum 8))
                      (define datum-desc : (Option String)
                        (case vbase
                          [(10) (number->string datum)]
                          [(16) (string-append "0x" (~r datum #:base '(up 16)))]
                          [(8)  (string-append "0"  (~r datum #:base 8))]
                          [else #false]))
                      
                      (define row : (Listof (Option Geo))
                        (cond [(symbol? self)
                               (list (geo-vr-append (geo-text self font #:color tcolor)
                                                    (geo-text address-desc font #:lines '(line-through) #:color igr-color))
                                     (geo-cc-superimpose (geo-rectangle #:id self #:stroke stroke #:fill (or tfill fill)
                                                                        grid-width grid-height)
                                                         (if (and datum-desc)
                                                             (geo-cc-superimpose
                                                              (geo-text binary-desc font #:lines '(line-through) #:color igr-color)
                                                              (geo-text datum-desc larger-font #:color tcolor))
                                                             (geo-text binary-desc font #:color tcolor))))]
                              [(keyword? self)
                               (let ([vname (keyword->immutable-string self)])
                                 (list (geo-vr-append (geo-text vname font #:color vcolor)
                                                      (geo-text address-desc font #:lines '(line-through) #:color igr-color))
                                       (geo-cc-superimpose (geo-rectangle #:id (string->symbol vname) #:stroke stroke #:fill (or vfill fill)
                                                                          grid-width grid-height)
                                                           (if (and datum-desc)
                                                               (geo-cc-superimpose
                                                                (geo-text binary-desc font #:lines '(line-through) #:color igr-color)
                                                                (geo-text datum-desc larger-font #:color vcolor))
                                                               (geo-text binary-desc font #:color vcolor)))))]
                              [else ; padded bytes
                               (list (geo-text address-desc font)
                                     (geo-cc-superimpose (geo-rectangle grid-width grid-height #:stroke stroke #:fill (or rfill fill))
                                                         (geo-text binary-desc font #:color rcolor)))]))
                      (gen-row (+ idx 1) rest (cons row swor)))])))

    (geo-table* #:id id memory-units '(rc lc) 'cc gapsize 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-states : (->* (Dia-Memory-State-Procedure (Listof Dia-Memory-Variable))
                                 (Dia-Memory-State-Snapshot #:memory-head (U Bytes Index) #:memory-tail (U Bytes Index) #:memset (U Boolean Byte Bytes))
                                 (Pairof Geo (Listof Geo)))
  (lambda [#:arrow-length [arrow-length -0.618]
           #:memory-head [head #""] #:memory-tail [tail #""] #:memset [memset #false]
           f variables [take-snapshot dia-memory-snapshot]]
    (define body-idx : Index (if (bytes? head) (bytes-length head) head))
    (define tail-idx : Nonnegative-Fixnum (+ body-idx (length variables)))
    (define memory-size : Nonnegative-Fixnum (+ body-idx (length variables) (if (bytes? tail) (bytes-length tail) tail)))
    (define addresses : (Immutable-HashTable Dia-Memory-Variable Integer)
      (for/hasheq : (Immutable-HashTable Dia-Memory-Variable Integer)
        ([var (in-list variables)]
         [idx (in-naturals body-idx)])
        (values var idx)))

    (define memory : Bytes (make-bytes memory-size (if (byte? memset) memset 0)))
    
    (cond [(not memset)
           (for ([idx (in-range (bytes-length memory))])
             (bytes-set! memory idx (random 256)))]
          [(bytes? memset) (bytes-copy! memory 0 memset 0 (min memory-size (bytes-length memset)))])
    
    (when (bytes? head)
      (bytes-copy! memory 0 head 0 (bytes-length head)))
    
    (when (bytes? tail)
      (bytes-copy! memory tail-idx tail 0 (bytes-length tail)))
    
    (define (memset! [var : Dia-Memory-Variable] [v : Byte]) : Void
      (bytes-set! memory (hash-ref addresses var) v))

    (define (memref [var : Dia-Memory-Variable]) : Byte
      (bytes-ref memory (hash-ref addresses var)))

    (define flow-arrow : Geo (geo-arrow 8.0 (~length arrow-length)))
    
    (call-in-nested-thread
     (λ [] (let ([this-thread (current-thread)])
             (define task-evt : (Evtof Any) (wrap-evt (thread-receive-evt) (λ _ (thread-receive))))

             (define (yield [op : Symbol '||]) : Void
               (thread-send this-thread 'snapshot)
               (void (thread-receive)))

             (define task-thread : Thread
               (thread (λ [] (void (f memset! memref yield)
                                   (thread-send this-thread '#:done)
                                   (thread-receive)))))

             (define snapshot0 : Geo (take-snapshot memory null body-idx))
             (let wsl : (Pairof Geo (Listof Geo)) ([snapshots : (Listof Geo) null])
               (define operator (sync/enable-break task-evt))
               (define snapshot : Geo (take-snapshot memory variables body-idx)) 

               (thread-send task-thread 'continue)
               (if (eq? operator '#:done)
                   (cons snapshot0 (reverse snapshots))
                   (wsl (cons snapshot snapshots)))))))))
