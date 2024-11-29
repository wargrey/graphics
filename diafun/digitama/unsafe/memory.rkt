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
         (let ([datum (c-variable-datum v)])
           (and (box? datum)
                (real? (unbox datum))))
         (let ([name (c-variable-name v)])
           (or (symbol? name)
               (keyword? name)))
         (let ([decltype (c-variable-decltype v)])
           (or (not decltype)
               (symbol? decltype)))
         (exact-nonnegative-integer? (c-variable-addr1 v))
         (symbol? (c-variable-segment v)))))

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
             [(uchar uint8 uint8_t) _ubyte]
             [(intptr intptr_t) _intptr]
             [(uintptr uintptr_t) _uintptr]
             [(intmax intmax_t) _intmax]
             [(uintmax uintmax_t) _uintmax]
             [(ldouble) _longdouble]
             [else _byte])))

(define c-vname->symbol
  (lambda [name]
    (if (eq? (string-ref name 0) #\_)
        (string->symbol name)
        (string->keyword name))))

(define c-run-callbacks
  (lambda [deal-with-snapshot [deal-with-variable void] #:lookahead-size [ahead 0] #:lookbehind-size [behind 0] #:body-limit [limit 1024]]
    (define addr-spaces (make-hasheq))
    (define variables (make-hash))
    
    (define (watch-variable name typename addr0 segment)
      (define type (c-typename->ctype typename))
      (define size (ctype-sizeof type))
      (define addr1 (+ addr0 size))
      (define addr-space (hash-ref addr-spaces segment (λ [] #false)))

      (hash-set! addr-spaces segment
                 (if (pair? addr-space)
                     (cons (min addr0 (unsafe-car addr-space))
                           (max addr1 (unsafe-cdr addr-space)))
                     (cons addr0 addr1)))

      (displayln (list segment name (number->string addr0 16)))

      (let* ([vptr (cast addr0 _uintptr _pointer)]
             [self (make-variable addr0 (make-bytes size) (box (ptr-ref vptr type)) (c-vname->symbol name) typename addr1 segment)])
        (memmove (c-placeholder-raw self) 0 vptr 0 1 type)
        (hash-set! variables addr0 (cons type self))
        (deal-with-variable self)
        (void)))
    
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
                    (let* ([self (cdr vinfo)]
                           [p++ (memory-step* ptr (car vinfo) (c-placeholder-raw self) (c-variable-datum self))])
                      (collect p++ (c-variable-addr1 self) (cons self srav)))
                    (let pad ([count 1])
                      (define addr++ (+ addr count))
                      (cond [(not (hash-has-key? variables addr++)) (pad (+ count 1))]
                            [else (let-values ([(raw p++) (memory-step-for-bytes ptr _byte count)])
                                    (collect p++ addr++ (cons (make-pad addr raw) srav)))]))))
              (void (deal-with-snapshot (cons segment (cons state srav))))))))

    (values watch-variable take-snapshot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define c-rkt-run
  (lambda [master]
    (parameterize ([current-custodian (make-custodian)])
      (define crkt-path (place-channel-get master))
      (define unsafe-cfun (place-channel-get master))
      (define cargv (place-channel-get master))
      (define callbacks (place-channel-get master))
      (define config (place-channel-get master))

      (define-values (watch-variable take-snapshot)
        (c-run-callbacks #:lookahead-size (car config)
                         #:lookbehind-size (cadr config)
                         #:body-limit (caddr config)
                         (λ [snapshots] (place-channel-put master snapshots))))
      
      (define retcode
        (with-handlers ([exn:fail? values])
          (when (pair? callbacks)
            (define watch! (dynamic-require crkt-path (unsafe-car callbacks) (λ [] #false)))
            (define take!  (dynamic-require crkt-path (unsafe-cdr callbacks) (λ [] #false)))
            
            (when (procedure? watch!) (watch! watch-variable))
            (when (procedure? take!)  (take!  take-snapshot)))
          
          (define cfun (dynamic-require crkt-path unsafe-cfun))
          
          (if (null? cargv)
              (if (procedure-arity-includes? cfun 2)
                  (cfun watch-variable take-snapshot)
                  (cfun))
              (apply cfun
                     (if (procedure-arity-includes? cfun (+ (length cargv) 2))
                         (append cargv (list watch-variable take-snapshot))
                         cargv)))))

      (when (exn? retcode)
        (place-channel-put master (exn-message retcode))
        (exit 95 #;'FATAL))
          
      (place-channel-put master retcode)
      retcode)))

(define c-run
  (lambda [master]
    (parameterize ([current-custodian (make-custodian)])
      (define c.so (place-channel-get master))
      (define unsafe-cfun (place-channel-get master))
      (define cargv (place-channel-get master))
      (define callbacks (place-channel-get master))
      (define config (place-channel-get master))
      
      (define-values (watch-variable take-snapshot)
        (c-run-callbacks #:lookahead-size (car config)
                         #:lookbehind-size (cadr config)
                         #:body-limit (caddr config)
                         (λ [snapshots] (place-channel-put master snapshots))))
      
      (define retcode
        (with-handlers ([exn:fail? values])
          (define c.dylib (ffi-lib c.so #:custodian (current-custodian)))
          (define cfun (get-ffi-obj unsafe-cfun c.dylib (_fun -> _int)))
          
          (when (pair? callbacks)
            (set-ffi-obj! (unsafe-car callbacks) c.dylib _watch_variable_t watch-variable)
            (set-ffi-obj! (unsafe-cdr callbacks) c.dylib _take_memory_snapshot_t take-snapshot))
          
          (cfun)))

      (when (exn? retcode)
        (place-channel-put master (exn-message retcode))
        (exit 95 #;'FATAL))
          
      (place-channel-put master retcode)
      retcode)))
