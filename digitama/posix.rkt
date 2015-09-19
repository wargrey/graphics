#lang at-exp racket

(provide (all-defined-out) ctype-basetype ctype-c->scheme ctype-scheme->c)
(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))
(provide (all-from-out ffi/unsafe/alloc))

@require{digicore.rkt}

(require syntax/location)
(require (for-syntax syntax/parse))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)
(require (only-in '#%foreign ctype-basetype ctype-c->scheme ctype-scheme->c))

(struct exn:foreign exn:fail (errno))

(define-syntax (digimon-ffi-lib stx)
  (syntax-parse stx #:literals []
    [(_ libname (~optional (~seq #:global? ?:expr)))
     #`(ffi-lib #:global? (not (not #,(attribute ?)))
                (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
                            (car (use-compiled-file-paths)) "native" (system-library-subpath #false) libname))]))

(define cvoid*?
  (lambda [v]
    (and v (cpointer? v))))

(define c-extern
  (lambda [variable ctype]
    (get-ffi-obj variable #false ctype)))

(define c-extern/enum
  ;;; racket->c can map multi names to one value, while c->racket uses the last name
  ;;; names in aliases will not be the value of c->racket
  (lambda [symbols #:map-symbol [symmap string-downcase]]
    (_enum (foldl (lambda [c Es] (list* (string->symbol (symmap (~a c))) '= (get-ffi-obj c #false _ufixint) Es)) null symbols))))

(define c-extern/bitmask
  (lambda [symbols #:map-symbol [symmap string-downcase]]
    (_bitmask (foldl (lambda [c Bs] (list* (string->symbol (symmap (~a c))) '= (get-ffi-obj c #false _uint) Bs)) null symbols))))

(define raise-foreign-error
  (lambda [src errno #:strerror [error->string strerror]]
    (raise (exn:foreign (format "~a: ~a; errno = ~a." src (error->string errno) errno)
                        (current-continuation-marks)
                        errno))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ffi-definer define-posix (ffi-lib #false))
(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true))

(define-posix strerror
  (_fun [errno : _int]
        [buffer : (_bytes o 32)]
        [size : _size = 32]
        -> _int
        -> (bytes->string/utf-8 (car (regexp-match #px"^[^\u0]*" buffer))))
  #:c-id strerror_r)

;;; Users and Groups

(define-posix getuid (_fun -> _uint32))
(define-posix getgid (_fun -> _uint32))
(define-posix geteuid (_fun -> _uint32))
(define-posix getegid (_fun -> _uint32))
(define-posix getppid (_fun -> _int32))
(define-posix getpid (_fun -> _int32))

(define-posix setuid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setuid (saved-errno)))))

(define-posix setgid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setgid (saved-errno)))))

(define-posix seteuid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'seteuid (saved-errno)))))

(define-posix setegid
  (_fun #:save-errno 'posix
        _uint32
        -> [$? : _int]
        -> (unless (zero? $?)
             (raise-foreign-error 'setegid (saved-errno)))))

(define-digitama fetch_tamer_ids
  (_fun #:save-errno 'posix
        [username : _bytes]
        [uid : (_ptr o _uint32)]
        [gid : (_ptr o _uint32)]
        -> [$? : _int]
        -> (cond [(zero? $?) (values uid gid)]
                 [else (raise-foreign-error 'fetch_tamer_ids $?)])))

(define-digitama fetch_tamer_name
  (_fun #:save-errno 'posix
        [uid : _uint32]
        [username : (_ptr o _bytes)]
        -> [$? : _int]
        -> (cond [(zero? $?) username]
                 [else (raise-foreign-error 'fetch_tamer_name $?)])))

(define-digitama fetch_tamer_group
  (_fun #:save-errno 'posix
        [gid : _uint32]
        [groupname : (_ptr o _bytes)]
        -> [$? : _int]
        -> (cond [(zero? $?) groupname]
                 [else (raise-foreign-error 'fetch_group_name $?)])))

;;; syslog
(define _logflags (c-extern/bitmask (list 'PID 'CONS 'ODELAY 'NDELAY 'NOWAIT)))
(define _severity (c-extern/enum (list 'EMERG 'ALERT 'FATAL 'ERROR 'WARNING 'NOTICE 'INFO 'DEBUG)))
(define _facility (c-extern/enum (list 'KERNEL 'USER 'MAIL 'DAEMON 'AUTH 'SYSLOG 'LPR 'NEWS
                                       'UUCP 'ALTCRON 'AUTHPRIV 'FTP 'NTP 'AUDIT 'CONSOLE 'CRON
                                       'LOCAL0 'LOCAL1 'LOCAL2 'LOCAL3 'LOCAL4 'LOCAL5 'LOCAL6 'LOCAL7)))

(define-posix openlog (_fun [identity :  _string] _logflags _facility -> _void))
(define-posix syslog (_fun _severity [message : _string] -> _void))
(define-posix setlogmask_one (_fun _severity -> _void))
(define-posix setlogmask_upto (_fun _severity -> _void))
(define-posix closelog (_fun -> _void))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))
  
  (require/typed/provide (submod "..")
                         [#:opaque CPointer/Null cpointer?]
                         [#:opaque CType ctype?]
                         [#:opaque Array array?]
                         [ctype-basetype (-> CType (U False Symbol CType (Listof CType)))]
                         [ctype-c->scheme (-> CType (All [c r] (-> c r)))]
                         [ctype-scheme->c (-> CType (All [c r] (-> r c)))]
                         [array-type (-> Array CType)]
                         [array-length (-> Array Index)]
                         [in-array (->* [Array] [Positive-Index (Option Positive-Index) Positive-Integer] (Sequenceof Any))]
                         [array-ref (-> Array Index * Any)]
                         [array-set! (case-> [Array Index Any -> Void]
                                             [Array Index Index Any -> Void]
                                             [Array Index Index Index Any -> Void])])
  
  (define-type CEnum Integer)
  (define-type REnum Symbol)
  (define-type CBitmask Integer)
  (define-type RBitmask (Listof Symbol))
  
  (define-type CEnum-C->Racket (-> CEnum REnum))
  (define-type CEnum-Racket->C (-> REnum CEnum))

  (define-type CBitmask-C->Racket (-> CBitmask RBitmask))
  (define-type CBitmask-Racket->C (-> RBitmask CBitmask))
  
  (require/typed/provide (submod "..")
                         [#:opaque CPointer cvoid*?]
                         [#:struct (exn:foreign exn:fail) ([errno : Integer])]
                         [c-extern (-> (U String Bytes Symbol) CType Any)])
  
  (require/typed/provide (submod "..")
                         [strerror (-> Natural String)]
                         [getuid (-> Natural)]
                         [getgid (-> Natural)]
                         [geteuid (-> Natural)]
                         [getegid (-> Natural)]
                         [seteuid (-> Natural Void)]
                         [setegid (-> Natural Void)]
                         [fetch_tamer_ids (-> Bytes (Values Natural Natural))]
                         [fetch_tamer_name (-> Natural Bytes)]
                         [fetch_tamer_group (-> Natural Bytes)]
                         [openlog (-> String RBitmask REnum Void)]
                         [syslog (-> Symbol String Void)]
                         [setlogmask_one (-> Symbol Void)]
                         [setlogmask_upto (-> Symbol Void)]
                         [closelog (-> Void)]))
