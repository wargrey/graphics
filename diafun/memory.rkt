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
(require geofun/digitama/dc/text)
(require geofun/digitama/layer/type)

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
  (lambda [#:id [id : (Option Symbol) #false] #:gapsize [gapsize : Real 8.0]
           #:ignore-variable? [ignore-variable? : Boolean #false] #:reverse-address? [reverse? : Boolean #true]
           #:address-mask [addr-mask : (Option Natural) (default-memory-address-mask)]
           #:address-human-readable? [addr-readable? : Boolean (default-memory-address-human-readable?)]
           #:padding-limit [padding-limit : (Option Index) (default-memory-padding-limit)]
           #:no-padding? [no-padding? : Boolean (default-memory-no-padding?)]
           #:combine-datum? [combine-datum? (default-memory-combine-datum?)]
           #:padding-radix [p-radix : Byte (default-memory-padding-radix)]
           #:fixnum-radix [fx-radix : Byte (default-memory-fixnum-radix)]
           #:raw-data-radix [rd-radix : Byte (default-memory-raw-data-radix)]
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
                         (make-placeholder rest (append swor (dia-padding-raw style address addr-mask addr-readable? raw p-radix padding-limit))))]
                    [(c-variable? self)
                     (let ([rows (if (and combine-datum?)
                                     (dia-variable-datum #:segment (c-variable-segment self) #:rendering-segment segment
                                                         style vname address addr-mask addr-readable? (unbox (c-variable-datum self)) fx-radix)
                                     (dia-variable-raw #:segment (c-variable-segment self) #:rendering-segment segment
                                                       style vname address addr-mask addr-readable? raw rd-radix))])
                       (make-placeholder rest (append swor rows)))]
                    [(c-vector? self)
                     (let ([rows (if (and combine-datum?)
                                     (dia-variable-data #:segment (c-vector-segment self) #:rendering-segment segment
                                                        style vname address (c-vector-type-size self) addr-mask addr-readable?
                                                        (c-vector-data self) fx-radix)
                                     (dia-variable-raw #:segment (c-vector-segment self) #:rendering-segment segment
                                                       style vname address addr-mask addr-readable? raw rd-radix))])
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
(define dia-memory-snapshots : (->* (Module-Path)
                                    (Dia-Memory-Snapshot #:main Symbol #:c-argv (Listof Any) #:take_snapshot Symbol
                                                         #:register_variable Symbol #:register_array Symbol
                                                         #:optimize? Boolean #:lookahead-size Byte #:lookbehind-size Byte #:body-limit Index)
                                    Dia-Memory-Snapshots)
  (lambda [#:main [cfun-name 'main] #:c-argv [cargv null] #:take_snapshot [take 'take_snapshot]
           #:register_variable [register-variable 'register_variable] #:register_array [register-array 'register_array]
           #:optimize? [optimize? #false] #:lookahead-size [ahead 0] #:lookbehind-size [behind 0] #:body-limit [limit 1024]
           crkt [take-snapshot dia-memory-snapshot]]
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
(define dia-memory-snapshots->table : (->* (Dia-Memory-Snapshots)
                                           (#:id (Option Symbol) #:segments (Listof Symbol) #:reverse-address? Boolean
                                            #:no-row-names? Boolean #:make-row-label (-> Symbol Geo)
                                            #:no-column-names? Boolean #:make-column-label (-> String Geo)
                                            #:gapsize Real)
                                           Geo)
  (lambda [#:id [id #false] #:segments [specific-segments null]
           #:make-row-label [make-row-label geo-text] #:make-column-label [make-col-label geo-text]
           #:no-row-names? [no-row-name? #false] #:no-column-names? [no-col-name? #false]
           #:gapsize [gapsize 16.0] #:reverse-address? [reverse? : Boolean #true]
           all-snapshots]
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

    (if (not headers)
        (geo-table* #:id id
                    rows
                    '(rc) (if (not has-stack?) (list 'cb) (list 'ct 'cb))
                    gapsize gapsize)
        (geo-table* #:id id
                    (cons (if (and no-row-name?) headers (cons #false headers)) rows)
                    '(cc rc) (if (not has-stack?) (list 'cc 'cb) (list 'cc 'ct 'cb))
                    gapsize gapsize))))
