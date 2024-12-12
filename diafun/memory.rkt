#lang typed/racket/base

(provide (all-defined-out))
(provide dia:memory? Dia:Memory)
(provide (all-from-out "digitama/memory/style.rkt"))
(provide (all-from-out "digitama/memory/interface.rkt"))

(require digimon/filesystem)

(require racket/list)
(require racket/place)

(require geofun/composite)
(require geofun/constructor)
(require geofun/digitama/convert)

(require "digitama/memory/dc.rkt")
(require "digitama/memory/exec.rkt")
(require "digitama/memory/self.rkt")
(require "digitama/memory/style.rkt")
(require "digitama/memory/interface.rkt")
(require "digitama/memory/identifier.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Dia-Reversed-Variables C-Variables)
(define-type Dia-Memory-Snapshot (-> Dia-Reversed-Variables Symbol String Geo))
(define-type Dia-Memory-Snapshots (Immutable-HashTable Symbol (Listof Geo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshot
  (lambda [#:id [id : (Option Symbol) #false] #:ignore-variable? [ignore-variable? : Boolean #false]
           #:gapsize [gapsize : Real (default-memory-location-gapsize)]
           #:reverse-address? [reverse? : Boolean (default-memory-reverse-address?)]
           #:address-mask [addr-mask : Natural (default-memory-address-mask)]
           #:padding-limit [padding-limit : (Option Index) (default-memory-padding-limit)]
           #:no-padding? [no-padding? : Boolean (default-memory-no-padding?)]
           #:human-readable? [human-readable? (default-memory-human-readable?)]
           #:padding-radix [p-radix : Positive-Byte (default-memory-padding-radix)]
           #:fixnum-radix [fx-radix : Positive-Byte (default-memory-fixnum-radix)]
           #:raw-data-radix [rd-radix : Positive-Byte (default-memory-raw-data-radix)]
           [variables : Dia-Reversed-Variables] [segment : Symbol 'stack] [state : String ""]] : Geo
    (parameterize ([default-dia-node-base-style make-memory-location-fallback-style])
      (let make-placeholder ([vars : (Listof C-Variable-Datum) variables]
                             [swor : (Listof (List Geo Geo)) null])
        (if (pair? vars)
            (let*-values ([(self rest) (values (car vars) (cdr vars))]
                          [(address raw) (values (c-placeholder-addr self) (c-placeholder-raw self))]
                          [(vname style) (memory-identify self segment)])
              (cond [(not style) (make-placeholder rest swor)]
                    [(or (c-padding? self) ignore-variable?)
                     (if (or no-padding?)
                         (make-placeholder rest swor)
                         (make-placeholder rest (append swor (dia-padding-raw style address addr-mask raw p-radix padding-limit))))]
                    [(c-variable? self)
                     (let ([rows (if (not human-readable?)
                                     (dia-variable-raw #:segment (c-variable-segment self) #:rendering-segment segment
                                                       style vname address addr-mask raw rd-radix)
                                     (dia-variable-datum #:segment (c-variable-segment self) #:rendering-segment segment
                                                         style vname address addr-mask (unbox (c-variable-datum self)) fx-radix))])
                       (make-placeholder rest (append swor rows)))]
                    [(c-vector? self)
                     (let ([rows (if (not human-readable?)
                                     (dia-vector-raw #:segment (c-vector-segment self) #:rendering-segment segment
                                                     style vname address (c-vector-type-size self) addr-mask raw rd-radix)
                                     (dia-variable-data #:segment (c-vector-segment self) #:rendering-segment segment
                                                        style vname address (c-vector-type-size self) addr-mask
                                                        (c-vector-data self) fx-radix))])
                       (make-placeholder rest (append swor rows)))]
                    [else (make-placeholder rest swor)]))
            
            (let ([rows (if reverse? swor (reverse swor))]
                  [cont (geo-blank)])
              (cond [(null? rows) cont]
                    [else (let ([addr1 (c-placeholder-addr (car variables))]
                                [addr2 (c-placeholder-addr (last variables))])
                            (make-dia:memory rows id '(rc lc) 'cc gapsize segment state
                                             (if (<= addr1 addr2) (cons addr1 addr2) (cons addr2 addr1))))])))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshots
  (lambda [#:main [cfun-name : Symbol (default-memory-entry)] #:c-argv [cargv : (Listof Any) null]
           #:take_snapshot [take : Symbol 'take_snapshot]
           #:register_variable [register-variable : Symbol 'register_variable]
           #:register_array [register-array : Symbol 'register_array]
           #:optimize? [optimize? : Boolean (default-memory-optimize?)]
           #:lookahead-size [ahead : Index (default-memory-lookahead-size)]
           #:lookbehind-size [behind : Index (default-memory-lookbehind-size)]
           #:body-limit [limit : Index (default-memory-body-limit)]
           [crkt : Module-Path] [take-snapshot : Dia-Memory-Snapshot dia-memory-snapshot]] : Dia-Memory-Snapshots
    (define modpath : Module-Path
      (cond [(not (or (path? crkt) (string? crkt))) crkt]
            [(not (regexp-match? #px"\\.rkt$" crkt)) (path->smart-absolute-path crkt)]
            [(path? crkt) `(file ,(path->string crkt))]
            [(string? crkt) `(file ,crkt)]
            [else crkt]))

    (define ghostcat (dynamic-place 'diafun/digitama/unsafe/memory (if (path? modpath) 'c-run 'c-rkt-run)))

    (place-channel-put ghostcat (if (path? modpath) (c-build modpath optimize?) modpath))
    (place-channel-put ghostcat cfun-name)
    (place-channel-put ghostcat cargv)
    (place-channel-put ghostcat (vector-immutable take register-variable register-array))
    (place-channel-put ghostcat (list ahead behind limit))

    (parameterize ([default-c-source (and (path? modpath) modpath)])
      (let wsl ([snapshots : (Immutable-HashTable Symbol (Listof Geo)) (hasheq)])
        (define message (sync/enable-break ghostcat))
        
        (cond [(c-memory-segment-snapshot? message)
               (wsl (hash-set snapshots (car message)
                              (cons (take-snapshot (cddr message) (car message) (cadr message))
                                    (hash-ref snapshots (car message) (inst list Geo)))))]
              [(not (string? message))
               (for/hasheq : (Immutable-HashTable Symbol (Listof Geo)) ([(segment geos) (in-hash snapshots)])
                 (values segment (reverse geos)))]
              [else (error 'dia-memory-snapshots "~a" message)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dia-memory-snapshots->table
  (lambda [#:id [id : (Option Symbol) #false]
           #:segments [specific-segments : (Listof Symbol) null] #:reverse-address? [reverse? : Boolean (default-memory-reverse-address?)]
           #:make-row-label [make-row-label : (-> Symbol Geo) dia-memory-table-label] #:no-row-names? [no-row-name? : Boolean #false]
           #:make-column-label [make-col-label : (-> String Geo) dia-memory-table-label] #:no-column-names? [no-col-name? : Boolean #false]
           #:segment-gapsize [segment-gapsize : Real (default-memory-segment-gapsize)]
           #:gapsize [gapsize : Real (default-memory-snapshot-gapsize)]
           [all-snapshots : Dia-Memory-Snapshots]] : Geo
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
           (for/list : (Listof (Option Geo)) ([memory (in-list (car (hash-values all-snapshots)))])
             (and (dia:memory? memory)
                  (make-col-label (dia:memory-state memory))))))

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
