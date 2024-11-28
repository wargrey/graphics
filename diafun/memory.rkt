#lang typed/racket/base

(provide (all-defined-out))

(require racket/keyword)
(require racket/place)

(require digimon/metrics)

(require geofun/font)
(require geofun/paint)
(require geofun/composite)
(require geofun/digitama/base)
(require geofun/digitama/convert)

(require "digitama/memory/interface.rkt")
(require "digitama/memory/byte-bag.rkt")
(require "digitama/memory/exec.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Reversed-Variables C-Variables)
(define-type Dia-Memory-Snapshot (-> Dia-Reversed-Variables Geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshot
  (lambda [#:id [id : (Option Symbol) #false] #:gapsize [gapsize : Real 8.0] #:reverse-address? [reverse? : Boolean #true]
           #:address-offset [address0 : Index #x20000] #:no-padding? [no-padding? : Boolean #false]
           #:no-binary-datum? [no-binary? #false] #:integer-base [fxbase : Byte 10] #:memory-base [mbase : Byte 16]
           #:font [font : Font (desc-font #:family 'monospace)] #:grid-width [width : Real -10.0] #:grid-height [height : Real -2.8]
           #:memory-stroke [stroke : Maybe-Stroke-Paint 'Black] #:memory-fill [fill : Maybe-Fill-Paint 'GhostWhite]
           #:variable-color [vcolor : Color 'ForestGreen] #:variable-fill [vfill : (Option Color) #false]
           #:temp-color [tcolor : Color 'DodgerBlue] #:temp-fill [tfill : (Option Color) #false]
           #:pad-color [pcolor : Color 'DimGray] #:pad-fill [pfill : (Option Color) 'LightGray]
           #:address-color [acolor : Color 'Black] #:ignored-color [igr-color : Color 'LightGray]
           [variables : Dia-Reversed-Variables]] : Geo
    (define-values (byte-width byte-height) (text-size "0" font))
    (define larger-font : Font (desc-font font #:size 'larger))
    (define grid-width  (max (~length width  byte-width)  (* byte-width 10.0)))
    (define grid-height (max (~length height byte-height) (* byte-height 2.0)))

    (define memory-units : (Listof (Listof (Option Geo)))
      (let gen-row : (Listof (Listof (Option Geo))) ([vars : (Listof (U C-Variable C-Pad)) variables]
                                                     [swor : (Listof (Listof (Option Geo))) null])
        (cond [(null? vars) (if reverse? swor (reverse swor))]
              [else (let*-values ([(self rest) (values (car vars) (cdr vars))]
                                  [(address raw) (values (c-placeholder-addr self) (c-placeholder-raw self))])
                      (cond [(c-padding? self)
                             (cond [(and no-padding?) (gen-row rest swor)]
                                   [else (let ([rows (dia-padding-bytes #:color pcolor #:stroke stroke #:fill (or pfill fill) #:address-color acolor
                                                                        grid-width grid-height address raw font)])
                                           (gen-row rest (append swor rows)))])]
                            [(keyword? (c-variable-name self))
                             (let ([vname (string->symbol (keyword->immutable-string (c-variable-name self)))])
                               (define rows : (Listof (Listof (Option Geo)))
                                 (if (and no-binary?)
                                     (dia-variable-datum #:color vcolor #:stroke stroke #:fill (or vfill fill) #:ignored-color igr-color
                                                         grid-width grid-height byte-width vname address (unbox (c-variable-datum self)) fxbase font larger-font)
                                     (dia-variable-raw #:color vcolor #:stroke stroke #:fill (or vfill fill) #:ignored-color igr-color
                                                       grid-width grid-height vname address raw mbase font larger-font)))
                               (gen-row rest (append swor rows)))]
                            [else ; temp variable
                             (let ([vname (c-variable-name self)])
                               (define rows : (Listof (Listof (Option Geo)))
                                 (if (and no-binary?)
                                     (dia-variable-datum #:color tcolor #:stroke stroke #:fill (or tfill fill) #:ignored-color igr-color
                                                         grid-width grid-height byte-width vname address (unbox (c-variable-datum self)) fxbase font larger-font)
                                     (dia-variable-raw #:color tcolor #:stroke stroke #:fill (or tfill fill) #:ignored-color igr-color
                                                       grid-width grid-height vname address raw mbase font larger-font)))
                               (gen-row rest (append swor rows)))]))])))

    (geo-table* #:id id memory-units '(rc lc) 'cc gapsize 0.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshots : (->* (Module-Path Symbol)
                                    (Dia-Memory-Snapshot #:c-argv (Listof Any) #:callback-names (Option (Pairof Symbol Symbol)) #:optimize? Boolean)
                                    (Listof Geo))
  (lambda [#:c-argv [cargv null] #:callback-names [callbacks '(watch_variable . take_snapshot)] #:optimize? [optimize? #true]
           crkt cfun-name [take-snapshot dia-memory-snapshot]]
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

    (let wsl : (Listof Geo) ([snapshots : (Listof Geo) null])
      (define message (sync/enable-break ghostcat))
      
      (cond [(c-memory-snapshot? message) (wsl (cons (take-snapshot (cdr message)) snapshots))]
            [(string? message) (error 'dia-memory-snapshots "~a" message)]
            [else (reverse snapshots)]))))
