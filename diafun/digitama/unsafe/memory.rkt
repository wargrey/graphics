#lang racket/base

(provide (all-defined-out))
(provide (struct-out c-variable))
(provide c-variable-display c-variable-displayln)

(require racket/case)
(require racket/place)
(require racket/unsafe/ops)

(require digimon/ffi)

(require "../memory/variable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-variable*?
  (lambda [v]
    (and (c-variable? v)
         (exact-nonnegative-integer? (c-placeholder-addr v))
         (bytes? (c-placeholder-raw v))
         (box? (c-variable-datum v))
         (let ([name (c-variable-name v)])
           (or (symbol? name)
               (keyword? name)))
         (let ([decltype (c-variable-decltype v)])
           (or (not decltype)
               (symbol? decltype)))
         (exact-nonnegative-integer? (c-variable-addr1 v))
         (symbol? (c-variable-segment v)))))

(define c-vector*?
  (lambda [v]
    (and (c-vector? v)
         (exact-nonnegative-integer? (c-placeholder-addr v))
         (bytes? (c-placeholder-raw v))
         (vector? (c-vector-data v))
         (let ([name (c-vector-name v)])
           (or (symbol? name)
               (keyword? name)))
         (let ([decltype (c-vector-decltype v)])
           (or (not decltype)
               (symbol? decltype)))
         (exact-nonnegative-integer? (c-vector-addr1 v))
         (byte? (c-vector-type-size v))
         (symbol? (c-vector-segment v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-typename->ctype
  (lambda [typename]
    (case/eq typename
             [(double flonum) _double]
             [(float) _float]
             [(long int64 sint64 int64_t sint64_t) _int64]
             [(ulong uint64 uint64_t) _uint64]
             [(int int32 sint32 int32_t sint32_t) _int32]
             [(uint uint32 uint32_t) _uint32]
             [(short word sword int16 sint16 int16_t sint16_t) _int16]
             [(ushort uword uint16 uint16_t) _uint16]
             [(int8 sint8 int8_t sint8_t) _sbyte]
             [(uint8 uint8_t) _ubyte]
             [(intptr intptr_t) _intptr]
             [(uintptr uintptr_t) _uintptr]
             [(intmax intmax_t) _intmax]
             [(uintmax uintmax_t) _uintmax]
             [(ldouble) _longdouble]
             [(uchar uchar_t) _uchar]
             [(char char_t) _char]
             [else _byte])))

(define c-vname->symbol
  (lambda [name type]
    (if (eq? type _uintptr)
        (string->keyword name)
        (string->symbol name))))

(define c-run-callbacks
  (lambda [deal-with-snapshot [deal-with-variable void] #:lookahead-size [ahead 0] #:lookbehind-size [behind 0] #:body-limit [limit 1024]]
    (define addr-spaces (make-hasheq))
    (define variables (make-hash))

    (define (take-snapshot state)
      (for ([(segment addr-space) (in-hash addr-spaces)])
        (define-values (start end) (values (unsafe-car addr-space) (unsafe-cdr addr-space)))
        (define addr0 (- start behind))
        (define addr$ (+ (if (> limit 0) (min end (+ start limit)) end) ahead))
        (define ptr0 (cast addr0 _uintptr _pointer))

        (let collect ([ptr ptr0]
                      [addr addr0]
                      [srav null])
          (if (< addr addr$)
              (let* ([vinfo (hash-ref variables addr (λ [] #false))])
                (if (pair? vinfo)
                    (let-values ([(type self) (values (unsafe-car vinfo) (unsafe-cdr vinfo))])
                      (cond [(c-vector? self)
                             (let ([p++ (memory-step* ptr (unsafe-car vinfo) (c-placeholder-raw self) (c-vector-data self))])
                               (collect p++ (c-vector-addr1 self) (unsafe-cons-list self srav)))]
                            [else ; normal variables
                             (let ([p++ (memory-step* ptr (unsafe-car vinfo) (c-placeholder-raw self) (c-variable-datum self))])
                               (collect p++ (c-variable-addr1 self) (unsafe-cons-list self srav)))]))
                    (let pad ([count 1])
                      (define addr++ (unsafe-fx+ addr count))
                      (if (or (hash-has-key? variables addr++) (>= #;'#:deadcode addr++ addr$))
                          (let-values ([(raw p++) (memory-step-for-bytes ptr _byte count)])
                            (collect p++ addr++ (unsafe-cons-list (make-pad addr raw) srav)))
                          (pad (unsafe-fx+ count 1))))))
              (void (deal-with-snapshot (unsafe-cons-list segment (unsafe-cons-list state srav))))))))
    
    (define (register-variable name typename addr0 segment)
      (define type (c-typename->ctype typename))
      (define size (ctype-sizeof type))
      (define addr1 (+ addr0 size))

      (c-update-address-range! addr-spaces segment addr0 addr1)
      (let* ([vptr (cast addr0 _uintptr _pointer)]
             [self (make-variable addr0 (make-bytes size) (box (ptr-ref vptr type)) (c-vname->symbol name type) typename addr1 segment)])
        #;(memmove (c-placeholder-raw self) 0 vptr 0 1 type)
        (hash-set! variables addr0 (cons type self))
        (deal-with-variable self)
        (void)))

    (define (register-array name typename addr0 segment length)
      (define type (c-typename->ctype typename))
      (define type-size (ctype-sizeof type))
      (define total (* type-size length))
      (define addr1 (+ addr0 total))

      (c-update-address-range! addr-spaces segment addr0 addr1)
      (let ([self (make-array addr0 (make-bytes total) (make-vector length) (c-vname->symbol name type) typename addr1 type-size segment)])
        #;(memmove (c-placeholder-raw self) 0 vptr 0 1 type)
        (hash-set! variables addr0 (cons type self))
        (deal-with-variable self)
        (void)))

    (values take-snapshot register-variable register-array)))

(define c-run-callbacks*
  (lambda [master config]
    (c-run-callbacks #:lookahead-size (car config)
                     #:lookbehind-size (cadr config)
                     #:body-limit (caddr config)
                     (λ [snapshots] (place-channel-put master snapshots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-rkt-run
  (lambda [master]
    (parameterize ([current-custodian (make-custodian)])
      (define crkt-path (place-channel-get master))
      (define unsafe-cfun (place-channel-get master))
      (define cargv (place-channel-get master))
      (define callbacks (place-channel-get master))
      (define config (place-channel-get master))
      (define-values (take-snapshot register-variable register-array) (c-run-callbacks* master config))
      
      (c-exit master
              (with-handlers ([exn:fail? values])
                (define take-snapshot!     (dynamic-require crkt-path (unsafe-vector*-ref callbacks 0) (λ [] #false)))
                (define register-variable! (dynamic-require crkt-path (unsafe-vector*-ref callbacks 1) (λ [] #false)))
                (define register-array!    (dynamic-require crkt-path (unsafe-vector*-ref callbacks 2) (λ [] #false)))
                
                (when (procedure? take-snapshot!)     (take-snapshot!     take-snapshot))
                (when (procedure? register-variable!) (register-variable! register-variable))
                (when (procedure? register-array!)    (register-array!    register-array))
                
                (define cfun (dynamic-require crkt-path unsafe-cfun))
                
                (if (pair? cargv)
                    (apply cfun cargv)
                    (cfun)))))))

(define c-run
  (lambda [master]
    (parameterize ([current-custodian (make-custodian)])
      (define c.so (place-channel-get master))
      (define unsafe-cfun (place-channel-get master))
      (define cargv (place-channel-get master))
      (define callbacks (place-channel-get master))
      (define config (place-channel-get master))
      (define-values (take-snapshot register-variable register-array) (c-run-callbacks* master config))
      
      (c-exit master
              (with-handlers ([exn:fail? values])
                (define c.dylib (ffi-lib c.so #:custodian (current-custodian)))
                (define cfun (get-ffi-obj unsafe-cfun c.dylib (_fun [argc : _int] [argv : (_list i _any_string)] -> _int)))
                
                (set-ffi-obj! (unsafe-vector*-ref callbacks 0) c.dylib _take_memory_snapshot_t take-snapshot)
                (set-ffi-obj! (unsafe-vector*-ref callbacks 1) c.dylib _register_variable_t register-variable)
                (set-ffi-obj! (unsafe-vector*-ref callbacks 2) c.dylib _register_array_t register-array)
                
                (cfun (unsafe-fx+ (length cargv) 1)
                      (cons c.so cargv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-update-address-range!
  (lambda [addr-spaces segment addr0 addr1]
    (define addr-space (hash-ref addr-spaces segment (λ [] #false)))
    
    (hash-set! addr-spaces segment
               (if (pair? addr-space)
                   (cons (min addr0 (unsafe-car addr-space))
                         (max addr1 (unsafe-cdr addr-space)))
                   (cons addr0 addr1)))))

(define c-exit
  (lambda [master retcode]
    (when (exn? retcode)
      (place-channel-put master (exn-message retcode))
      (exit 95 #;'FATAL))
    
    (place-channel-put master retcode)
    retcode))
