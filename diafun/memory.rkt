#lang typed/racket/base

(provide (all-defined-out))
(provide dia:ram? Dia:RAM)
(provide Dia-RAM-Variable-Layout dia-ram-variable-layout)
(provide (all-from-out "digitama/ram/style.rkt"))
(provide (all-from-out "digitama/ram/interface.rkt"))

(require digimon/filesystem)

(require racket/list)
(require racket/place)

(require geofun/composite)
(require geofun/constructor)
(require geofun/digitama/convert)

(require "digitama/ram/dc.rkt")
(require "digitama/ram/exec.rkt")
(require "digitama/ram/self.rkt")
(require "digitama/ram/style.rkt")
(require "digitama/ram/interface.rkt")
(require "digitama/ram/identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Reversed-Variables C-Variables)
(define-type Dia-RAM-Snapshot (-> Dia-Reversed-Variables Symbol String [#:layout Dia-RAM-Variable-Layout] (U Dia:RAM Geo:Blank)))
(define-type Dia-RAM-Snapshots (Immutable-HashTable Symbol (Listof (U Dia:RAM Geo:Blank))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-ram-snapshot
  (lambda [#:id [id : (Option Symbol) #false] #:ignore-variable? [ignore-variable? : Boolean #false]
           #:gapsize [gapsize : Real (default-ram-location-gapsize)]
           #:reverse-address? [reverse? : Boolean (default-ram-reverse-address?)]
           #:address-mask [addr-mask : Natural (default-ram-address-mask)]
           #:padding-limit [padding-limit : (Option Index) (default-ram-padding-limit)]
           #:no-padding? [no-padding? : Boolean (default-ram-no-padding?)]
           #:human-readable? [human-readable? (default-ram-human-readable?)]
           #:padding-radix [p-radix : Positive-Byte (default-ram-padding-radix)]
           #:fixnum-radix [fx-radix : Positive-Byte (default-ram-fixnum-radix)]
           #:raw-data-radix [rd-radix : Positive-Byte (default-ram-raw-data-radix)]
           #:layout [layout : Dia-RAM-Variable-Layout dia-ram-variable-layout]
           [variables : Dia-Reversed-Variables] [segment : Symbol 'stack] [state : String ""]] : (U Dia:RAM Geo:Blank)
    (parameterize ([default-dia-node-base-style make-ram-location-fallback-style])
      (define (realize [self : RAM-Variable]) : (Pairof Geo (Listof Geo))
        (layout (ram-variable-name self) (ram-variable-address self)
                (ram-variable-datum self) (ram-variable-shape self)))
      
      (let var->cell ([vars : (Listof C-Variable-Datum) variables]
                      [swor : (Listof (Pairof Geo (Listof Geo))) null])
        (if (pair? vars)
            (let*-values ([(self rest) (values (car vars) (cdr vars))]
                          [(address raw) (values (c-placeholder-addr self) (c-placeholder-raw self))]
                          [(vname style) (ram-identify self segment)])
              (cond [(not style) (var->cell rest swor)]
                    [(or (c-padding? self) ignore-variable?)
                     (if (or no-padding?)
                         (var->cell rest swor)
                         (var->cell rest (append swor (map realize (dia-padding-raw style address addr-mask raw p-radix padding-limit)))))]
                    [(c-variable? self)
                     (let ([rows (if (not human-readable?)
                                     (dia-variable-raw #:segment (c-variable-segment self) #:rendering-segment segment
                                                       style vname address addr-mask raw rd-radix)
                                     (dia-variable-datum #:segment (c-variable-segment self) #:rendering-segment segment
                                                         style vname address addr-mask (unbox (c-variable-datum self)) fx-radix))])
                       (var->cell rest (append swor (map realize rows))))]
                    [(c-vector? self)
                     (let ([rows (if (not human-readable?)
                                     (dia-vector-raw #:segment (c-vector-segment self) #:rendering-segment segment
                                                     style vname address (c-vector-type-size self) addr-mask raw rd-radix)
                                     (dia-variable-data #:segment (c-vector-segment self) #:rendering-segment segment
                                                        style vname address (c-vector-type-size self) addr-mask
                                                        (c-vector-data self) fx-radix))])
                       (var->cell rest (append swor (map realize rows))))]
                    [else (var->cell rest swor)]))
            
            (let ([rows (if reverse? swor (reverse swor))])
              (cond [(null? rows) (geo-blank)]
                    [else (let ([addr1 (c-placeholder-addr (car variables))]
                                [addr2 (c-placeholder-addr (last variables))])
                            (make-dia:ram rows id '(rc lc) 'cc gapsize segment state
                                          (if (<= addr1 addr2) (cons addr1 addr2) (cons addr2 addr1))))])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-ram-snapshots
  (lambda [#:main [cfun-name : Symbol (default-ram-entry)] #:c-argv [cargv : (Listof Any) null]
           #:take_snapshot [take : Symbol 'take_snapshot]
           #:register_variable [register-variable : Symbol 'register_variable]
           #:register_array [register-array : Symbol 'register_array]
           #:optimize? [optimize? : Boolean (default-ram-optimize?)]
           #:lookahead-size [ahead : Index (default-ram-lookahead-size)]
           #:lookbehind-size [behind : Index (default-ram-lookbehind-size)]
           #:body-limit [limit : Index (default-ram-body-limit)]
           #:layout [layout : Dia-RAM-Variable-Layout dia-ram-variable-layout]
           [crkt : Module-Path] [take-snapshot : Dia-RAM-Snapshot dia-ram-snapshot]] : Dia-RAM-Snapshots
    (define modpath : Module-Path
      (cond [(not (or (path? crkt) (string? crkt))) crkt]
            [(not (regexp-match? #px"\\.rkt$" crkt)) (path->smart-absolute-path crkt)]
            [(path? crkt) `(file ,(path->string crkt))]
            [(string? crkt) `(file ,crkt)]
            [else crkt]))

    (define ghostcat (dynamic-place 'diafun/digitama/unsafe/ram (if (path? modpath) 'c-run 'c-rkt-run)))

    (place-channel-put ghostcat (if (path? modpath) (c-build modpath optimize?) modpath))
    (place-channel-put ghostcat cfun-name)
    (place-channel-put ghostcat cargv)
    (place-channel-put ghostcat (vector-immutable take register-variable register-array))
    (place-channel-put ghostcat (list ahead behind limit))

    (parameterize ([default-c-source (and (path? modpath) modpath)])
      (let wsl ([snapshots : (Immutable-HashTable Symbol (Listof (U Dia:RAM Geo:Blank))) (hasheq)])
        (define message (sync/enable-break ghostcat))
        
        (cond [(c-ram-segment-snapshot? message)
               (wsl (hash-set snapshots (car message)
                              (cons (take-snapshot (cddr message) (car message) (cadr message) #:layout layout)
                                    (hash-ref snapshots (car message) (inst list (U Dia:RAM Geo:Blank))))))]
              [(not (string? message))
               (for/hasheq : (Immutable-HashTable Symbol (Listof (U Dia:RAM Geo:Blank))) ([(segment geos) (in-hash snapshots)])
                 (values segment (reverse geos)))]
              [else (error 'dia-ram-snapshots "~a" message)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-ram-snapshots->table
  (lambda [#:id [id : (Option Symbol) #false]
           #:segments [specific-segments : (Listof Symbol) null] #:reverse-address? [reverse? : Boolean (default-ram-reverse-address?)]
           #:make-row-label [make-row-label : (-> Symbol Geo) dia-ram-table-label] #:hide-segment-names? [no-col-name? : Boolean #false]
           #:make-column-label [make-col-label : (-> String Geo) dia-ram-table-label] #:hide-states? [no-row-name? : Boolean #false]
           #:segment-gapsize [segment-gapsize : Real (default-ram-segment-gapsize)]
           #:gapsize [gapsize : Real (default-ram-snapshot-gapsize)]
           [all-snapshots : Dia-RAM-Snapshots]] : Geo
    (define segments : (Listof Symbol)
      (for/list ([seg (in-list specific-segments)]
                 #:when (hash-has-key? all-snapshots seg))
        seg))
    
    (define key-segments : (Listof Symbol)
      ((inst sort Symbol Index) #:cache-keys? #true
                                #:key (λ [[seg : Symbol]] : Index (dia-snapshot-address (hash-ref all-snapshots seg (inst list Geo))))
                                (if (null? segments) (hash-keys all-snapshots) segments)
                                (if (not reverse?) < >)))

    (define rows : (Listof (Listof Geo))
        (if (and no-row-name?)
            (for/list ([seg (in-list key-segments)])
              (hash-ref all-snapshots seg))
            (for/list ([seg (in-list key-segments)])
              (cons (make-row-label seg)
                    (hash-ref all-snapshots seg)))))

    (define headers : (Option (Listof (Option Geo)))
      (and (not no-col-name?)
           (not (hash-empty? all-snapshots))
           (for/list : (Listof (Option Geo)) ([ram (in-list (car (hash-values all-snapshots)))])
             (and (dia:ram? ram)
                  (make-col-label (dia:ram-state ram))))))

    (define has-stack? (hash-has-key? all-snapshots 'stack))
    (define col-gaps (if (and no-row-name?) (list gapsize) (list segment-gapsize gapsize)))

    (if (pair? headers)
        (geo-table* #:id id
                    (cons (if (and no-row-name?) headers (cons #false headers)) rows)
                    '(rc) (if (not has-stack?) (list 'cb) (list 'cb 'ct 'cb))
                    col-gaps (list segment-gapsize))
        (geo-table* #:id id
                    rows
                    '(rc) (if (not has-stack?) (list 'cb) (list 'ct 'cb))
                    col-gaps (list segment-gapsize)))))
