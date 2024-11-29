#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)
(require racket/place)

(require digimon/metrics)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)
(require geofun/constructor)

(require geofun/digitama/base)
(require geofun/digitama/convert)
(require geofun/digitama/dc/text)
(require geofun/digitama/layer/type)

(require "digitama/memory/interface.rkt")
(require "digitama/memory/byte-bag.rkt")
(require "digitama/memory/exec.rkt")
(require "digitama/memory/dc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Reversed-Variables C-Variables)
(define-type Dia-Memory-Snapshot (-> Dia-Reversed-Variables Symbol String Geo))
(define-type Dia-Memory-Snapshots (Immutable-HashTable Symbol (Listof Geo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshot
  (lambda [#:id [id : (Option Symbol) #false] #:gapsize [gapsize : Real 8.0] #:reverse-address? [reverse? : Boolean #true]
           #:address-offset [address0 : Index #x20000] #:no-padding? [no-padding? : Boolean #false] #:no-binary-datum? [no-binary? #false]
           #:integer-base [fxbase : Byte 10] #:memory-base [mbase : Byte 16] #:padding-base [pbase : Byte 2]
           #:font [font : Font (desc-font #:family 'monospace)] #:grid-width [width : Real -10.0] #:grid-height [height : Real -2.8]
           #:memory-stroke [stroke : Maybe-Stroke-Paint 'Black] #:memory-fill [fill : Maybe-Fill-Paint 'GhostWhite]
           #:variable-color [vcolor : Color 'ForestGreen] #:variable-fill [vfill : (Option Color) #false]
           #:temp-color [tcolor : Color 'DodgerBlue] #:temp-fill [tfill : (Option Color) #false]
           #:pad-color [pcolor : Color 'DimGray] #:pad-fill [pfill : (Option Color) 'LightGray]
           #:address-color [acolor : Color 'Black] #:ignored-color [igr-color : Color 'LightGray]
           [variables : Dia-Reversed-Variables] [segment : Symbol 'stack] [state : String ""]] : Geo
    (define-values (byte-width byte-height) (text-size "0" font))
    (define larger-font : Font (desc-font font #:size 'larger))
    (define grid-width  (max (~length width  byte-width)  (* byte-width 10.0)))
    (define grid-height (max (~length height byte-height) (* byte-height 2.0)))

    (let make-memory ([vars : (Listof (U C-Variable C-Pad)) variables]
                      [swor : (Listof (List Geo Geo)) null])
      (if (pair? vars)
          (let*-values ([(self rest) (values (car vars) (cdr vars))]
                        [(address raw) (values (c-placeholder-addr self) (c-placeholder-raw self))])
            (cond [(c-padding? self)
                   (cond [(and no-padding?) (make-memory rest swor)]
                         [else (let ([rows (dia-padding-bytes #:color pcolor #:stroke stroke #:fill (or pfill fill) #:address-color acolor
                                                              grid-width grid-height address raw pbase font)])
                                 (make-memory rest (append swor rows)))])]
                  [(keyword? (c-variable-name self))
                   (let ([vname (string->symbol (keyword->immutable-string (c-variable-name self)))])
                     (define rows : (Listof (List Geo Geo))
                       (if (and no-binary?)
                           (dia-variable-datum #:color vcolor #:stroke stroke #:fill (or vfill fill) #:ignored-color igr-color
                                               #:segment (c-variable-segment self) #:rendering-segment segment
                                               grid-width grid-height byte-width vname address (unbox (c-variable-datum self)) fxbase font larger-font)
                           (dia-variable-raw #:color vcolor #:stroke stroke #:fill (or vfill fill) #:ignored-color igr-color
                                             #:segment (c-variable-segment self) #:rendering-segment segment
                                             grid-width grid-height vname address raw mbase font larger-font)))
                     (make-memory rest (append swor rows)))]
                  [else ; temp variable
                   (let ([vname (c-variable-name self)])
                     (define rows : (Listof (List Geo Geo))
                       (if (and no-binary?)
                           (dia-variable-datum #:color tcolor #:stroke stroke #:fill (or tfill fill) #:ignored-color igr-color
                                               #:segment (c-variable-segment self) #:rendering-segment segment
                                               grid-width grid-height byte-width vname address (unbox (c-variable-datum self)) fxbase font larger-font)
                           (dia-variable-raw #:color tcolor #:stroke stroke #:fill (or tfill fill) #:ignored-color igr-color
                                             #:segment (c-variable-segment self) #:rendering-segment segment
                                             grid-width grid-height vname address raw mbase font larger-font)))
                     (make-memory rest (append swor rows)))]))
          
          (let ([rows (if reverse? swor (reverse swor))]
                [cont (geo-blank)])
            (cond [(null? rows) cont]
                  [else (make-dia:memory rows id '(rc lc) 'cc gapsize segment state
                                         (cons 0 0))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshots : (->* (Module-Path)
                                    (Dia-Memory-Snapshot #:entry Symbol #:c-argv (Listof Any) #:callback-names (Option (Pairof Symbol Symbol)) #:optimize? Boolean
                                                         #:lookahead-size Byte #:lookbehind-size Byte #:body-limit Index)
                                    Dia-Memory-Snapshots)
  (lambda [#:c-argv [cargv null] #:callback-names [callbacks '(watch_variable . take_snapshot)] #:optimize? [optimize? #true] #:entry [cfun-name 'main]
           #:lookahead-size [ahead 0] #:lookbehind-size [behind 0] #:body-limit [limit 1024]
           crkt [take-snapshot dia-memory-snapshot]]
    (define modpath : Module-Path
      (cond [(not (or (path? crkt) (string? crkt))) crkt]
            [(not (regexp-match? #px"\\.rkt$" crkt)) (c-build (if (string? crkt) (string->path crkt) crkt) optimize?)]
            [(path? crkt) `(file ,(path->string crkt))]
            [(string? crkt) `(file ,crkt)]
            [else crkt]))
    
    (define ghostcat (dynamic-place 'diafun/digitama/unsafe/memory (if (path? modpath) 'c-run 'c-rkt-run)))

    (place-channel-put ghostcat modpath)
    (place-channel-put ghostcat cfun-name)
    (place-channel-put ghostcat cargv)
    (place-channel-put ghostcat callbacks)
    (place-channel-put ghostcat (list ahead behind limit))

    (let wsl ([snapshots : (Immutable-HashTable Symbol (Listof Geo)) (hasheq)])
      (define message (sync/enable-break ghostcat))
      
      (cond [(c-memory-segment-snapshot? message)
             (wsl (hash-set snapshots (car message)
                            (cons (take-snapshot (cddr message) (car message) (cadr message))
                                  (hash-ref snapshots (car message) (inst list Geo)))))]
            [(not (string? message))
             (for/hasheq : (Immutable-HashTable Symbol (Listof Geo)) ([(segment geos) (in-hash snapshots)])
               (values segment (reverse geos)))]
            [else (error 'dia-memory-snapshots "~a" message)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshots->table : (->* (Dia-Memory-Snapshots)
                                           (#:id (Option Symbol) #:gapsize Real #:row-alignment Geo-Pin-Anchor #:make-label (-> Symbol Geo))
                                           Geo)
  (lambda [all-snapshots #:id [id #false] #:gapsize [gapsize 16.0] #:row-alignment [ralign 'ct] #:make-label [make-label geo-text]]
    (define segments : (Listof Symbol) (hash-keys all-snapshots)
      #;((inst sort Symbol Real) #:cache-keys? #true
                               #:key? (λ [[seg : Symbol]] : Real 0)
                               (hash-keys all-snapshots)
                               <))
    
    (geo-table* #:id id
                (for/list : (Listof (Listof (Option Geo))) ([segname (in-list segments)])
                  (cons (make-label segname) (hash-ref all-snapshots segname)))
                '(rc rc) `(ct ,ralign) gapsize gapsize)))
