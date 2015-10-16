#lang at-exp racket

(provide (except-out (all-defined-out) define-posix define-digitama))
(provide ctype-basetype ctype-c->scheme ctype-scheme->c)

(provide (all-from-out ffi/unsafe))
(provide (all-from-out ffi/unsafe/define))
(provide (all-from-out ffi/unsafe/alloc))

@require{digicore.rkt}

(require (for-syntax syntax/parse))
(require (for-syntax racket/syntax))
(require (for-syntax racket/path))
(require (for-syntax racket/match))
(require (for-syntax racket/string))
(require (for-syntax racket/sequence))

(require ffi/unsafe)
(require ffi/unsafe/define)
(require ffi/unsafe/alloc)

(require (only-in '#%foreign ctype-basetype ctype-c->scheme ctype-scheme->c))

(struct exn:foreign exn:fail (errno strerror))
(struct exn:break:signal exn:break (signo))

(define-syntax (digimon-ffi-lib stx)
  (syntax-parse stx #:literals []
    [(_ libname (~optional (~seq #:global? ?:expr)))
     #`(ffi-lib #:global? (not (not #,(attribute ?)))
                (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
                            (car (use-compiled-file-paths)) "native" (system-library-subpath #false) libname))]))

(define-syntax (require-prefab-cstruct stx)
  (syntax-case stx []
    [(_ src.c) #| TODO: to see if we still have to provide all cstruct ids |#
     (with-syntax ([mod.c (format-id #'src.c "prefab:~a" (path->string (file-name-from-path (syntax-e #'src.c))))]
                   [([id id/bzero &id/bzero *id &id _!id ([field-id type-expr Type defvalue] ...) !id->list list->!id !id? id* id_t*] ...)
                    (for/list ([cstruct (in-list (let ([px.cstruct #px#"typedef\\s+struct\\s+ffi_prefab_\\w+.+?\\}\\s*\\w+\\s*(?=;)"])
                                                   (for/list ([src (in-list (parameterize ([current-directory (current-load-relative-directory)])
                                                                              (call-with-input-file (syntax-e #'src.c)
                                                                                (lambda [cin] (regexp-match* px.cstruct cin)))))])
                                                     (sequence->list (in-port read (open-input-bytes (regexp-replaces src
                                                                                                                      '([#px"ffi_prefab_" ""]
                                                                                                                        [#px"\\{" " (("]
                                                                                                                        [#px";\\s+(?!\\})" ") ("]
                                                                                                                        [#px";\\s+\\}" ")) "]))))))))])
                      (define cstruct-id (format "!~a" (list-ref cstruct 2))) ; for the prefab cstruct, this id is not useful as well as normal cstruct
                      (define pointer_t* (format "~a*" (list-ref cstruct 4))) ; for the prefab cstruct, this id should take the place of _id-pointer 
                      (define rstruct-id (format "~a" (list-ref cstruct 2)))  ; for the prefab cstruct, this id should take the first place
                      (with-syntax ([csid (format-id #'csid "~a" rstruct-id)]
                                    [csid/bzero (format-id #'csid/bzero "~a/bzero" rstruct-id)]
                                    [&csid/bzero (format-id #'&csid/bzero "&~a/bzero" rstruct-id)]
                                    [*csid (format-id #'*csid "*~a" rstruct-id)] ; in C, (*p) means get the value that p points to.
                                    [&csid (format-id #'&csid "&~a" rstruct-id)] ; in C, &v mean get the storage address of v
                                    [_csid (format-id #'_csid "_~a" cstruct-id)]
                                    [csid->list (format-id #'csid->list "~a->list" cstruct-id)]
                                    [list->csid (format-id #'list->csid "list->~a" cstruct-id)]
                                    [csid? (format-id #'csid? "~a?" cstruct-id)]
                                    [csid* (format-id #'csid* "_~a-pointer" cstruct-id)]
                                    [id_t* (format-id #'id_t* "~a" pointer_t*)]
                                    [([field-id _type Type defval] ...)
                                     (for/list ([definition (in-list (list-ref cstruct 3))])
                                       (define unsigned? (and (memq 'unsigned definition) #true))
                                       (match (remq* '(signed unsigned int) definition)
                                         [(list 'char field) #`(#,field (if #,unsigned? _ubyte _byte) Byte 0)]
                                         [(list field) #`(#,field (if #,unsigned? _uint _int) (if #,unsigned? Nonnegative-Fixnum Fixnum) 0)]
                                         [(list 'short field) #`(#,field (if #,unsigned? _ushort _short) (if #,unsigned? Index Fixnum) 0)]
                                         [(list 'intptr_t field) #`(#,field _intptr Integer 0)]
                                         [(list 'uintptr_t field) #`(#,field _uintptr Natural 0)]
                                         [(list 'long field) #`(#,field (if #,unsigned? _ulong _long) (if #,unsigned? Natural Integer) 0)]
                                         [(list 'long 'long field) #`(#,field (if #,unsigned? _ullong _llong) (if #,unsigned? Natural Integer) 0)]
                                         [(list 'float field) #`(#,field _float Single-Flonum 0.0)]
                                         [(list 'double field) #`(#,field _double Flonum 0.0)]
                                         [(list 'long 'double field) #`(#,field _longdouble Real 0.0)]
                                         [(list 'intmax_t field) #`(#,field _intmax Integer 0)]
                                         [(list 'uintmax_t field) #`(#,field _uintmax Natural 0)]
                                         [(list 'size_t field) #`(#,field _size Natural 0)]
                                         [(list 'ssize_t field) #`(#,field _ssize Integer 0)]
                                         [(list 'time_t field) #`(#,field _ullong Natural 0)]
                                         [(list 'ptrdiff_t field) #`(#,field _ptrdiff Integer 0)]
                                         [_ (raise-syntax-error 'require-prefab-cstruct (format "unknown ctype in ~a" definition) cstruct)]))])
                        #'[csid csid/bzero &csid/bzero *csid &csid _csid ([field-id _type Type defval] ...) csid->list list->csid csid? csid* id_t*]))])
       #'(module mod.c racket/base
           (provide (all-defined-out))
           
           (require ffi/unsafe)
           
           (define-cstruct _!id ([field-id type-expr] ...)) ...
           (define id_t* id*) ...
           
           (struct id ([field-id] ...) #:prefab) ...
           (define id/bzero (lambda [] (id defvalue ...))) ...

           (define *id (lambda [p] (apply id (!id->list p)))) ...
           (define &id (lambda [s] (list->!id (cdr (vector->list (struct->vector s)))))) ...
           (define &id/bzero (lambda [] (&id (id defvalue ...)))) ...

           (module* typed/ffi typed/racket/base
             (provide (except-out (all-defined-out) id_t* ...))

             (require/typed (submod "..")
                            [#:opaque id_t* !id?] ...)
             
             (require/typed/provide (submod "..")
                                    [#:struct id ([field-id : Type] ...)] ...
                                    [id/bzero (-> id)] ...
                                    [*id (-> id_t* id)] ...
                                    [&id (-> id id_t*)] ...
                                    [&id/bzero (-> id_t*)] ...))))]))

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
  (lambda [src errno #:strerror [errno->string strerror]]
    (raise (exn:foreign (format "~a: ~a; errno = ~a." src (errno->string errno) errno)
                        (current-continuation-marks)
                        errno errno->string))))

(define raise-signal-error
  (lambda [signame/no]
    ;;; TODO: make signo portable
    (define named-signals (hasheq 'SIGHUP 1 'SIGINT 2 'SIGQUIT 3 'SIGILL 4 'SIGTRAP 5 'SIGABRT 6 'SIGEMT 7 'SIGFPE 8
                                  'SIGKILL 9 'SIGBUS 10 'SIGSEGV 11 'SIGSYS 12 'SIGPIPE 13 'SIGALRM 14 'SIGTERM 15))
    (define signo (if (integer? signame/no) signame/no (hash-ref named-signals signame/no)))
    (let/ec collapse
      (raise (exn:break:signal (format "~a" (strsignal signo))
                               (current-continuation-marks)
                               collapse signo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-ffi-definer define-posix (ffi-lib #false))
(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true))

(define-posix strerror
  (_fun [errno : _int]
        [buffer : (_bytes o 32)]
        [size : _size = 32]
        -> _int
        -> (bytes->string/utf-8 (car (regexp-match #px"^[^\u0]*" buffer))))
  #:c-id strerror_r)

(define-posix gai_strerror
  (_fun [signo : _int]
        -> _string))

(define-posix strsignal
  (_fun [signo : _int]
        -> _string))

;;; Users and Groups

(define-posix getppid (_fun -> _int32))
(define-posix getpid (_fun -> _int32))
(define-posix getuid (_fun -> _uint32))
(define-posix getgid (_fun -> _uint32))
(define-posix geteuid (_fun -> _uint32))
(define-posix getegid (_fun -> _uint32))

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
(define _logflags (c-extern/bitmask (list 'PID 'CONS 'NDELAY 'NOWAIT)))

; these are defined in RFC5424, facility names are system dependent.
(define _facility (_enum (list 'kernel   '= (arithmetic-shift 00 3) #| kernel messages |#
                               'user     '= (arithmetic-shift 01 3) #| random user-level messages |#
                               'mail     '= (arithmetic-shift 02 3) #| mail system |#
                               'daemon   '= (arithmetic-shift 03 3) #| system daemons |#
                               'auth     '= (arithmetic-shift 04 3) #| security/authorization messages |#
                               'syslog   '= (arithmetic-shift 05 3) #| messages generated internally by syslogd |#
                               'lpr      '= (arithmetic-shift 06 3) #| line printer subsystem |#
                               'news     '= (arithmetic-shift 07 3) #| netnews subsystem |#
                               'uucp     '= (arithmetic-shift 08 3) #| uucp subsystem |#
                               'altcron  '= (arithmetic-shift 09 3) #| BSD cron/at subsystem |#
                               'authpriv '= (arithmetic-shift 10 3) #| BSD security/authorization messages |#
                               'ftp      '= (arithmetic-shift 11 3) #| file transfer subsystem |#
                               'ntp      '= (arithmetic-shift 12 3) #| network time subsystem |#
                               'audit    '= (arithmetic-shift 13 3) #| audit subsystem |#
                               'console  '= (arithmetic-shift 14 3) #| BSD console messages |#
                               'cron     '= (arithmetic-shift 15 3) #| cron/at subsystem |#
                               'local0   '= (arithmetic-shift 16 3) #| reserved for local use |#
                               'local1   '= (arithmetic-shift 17 3) #| reserved for local use |#
                               'local2   '= (arithmetic-shift 18 3) #| reserved for local use |#
                               'local3   '= (arithmetic-shift 19 3) #| reserved for local use |#
                               'local4   '= (arithmetic-shift 20 3) #| reserved for local use |#
                               'local5   '= (arithmetic-shift 21 3) #| reserved for local use |#
                               'local6   '= (arithmetic-shift 22 3) #| reserved for local use |#
                               'local7   '= (arithmetic-shift 23 3) #| reserved for local use |#)))

(define _severity (_enum (list 'emerg   '= 0 #| system is unusable |#
                               'alert   '= 1 #| action must be taken immediately |#
                               'fatal   '= 2 #| critical conditions |#
                               'error   '= 3 #| error conditions |#
                               'warning '= 4 #| warning conditions |#
                               'notice  '= 5 #| normal but significant condition |#
                               'info    '= 6 #| informational |#
                               'debug   '= 7 #| debug-level messages |#)))

(define-posix openlog (_fun [identity :  _string] _logflags _facility -> _void))
(define-posix syslog (_fun _severity [message : _string] -> _void))
(define-posix setlogmask_one (_fun _severity -> _void))
(define-posix setlogmask_upto (_fun _severity -> _void))
(define-posix closelog (_fun -> _void))

;;; system monitor
(define _sysconf (c-extern/enum #:map-symbol values (list 'NPROCESSORS_CONF 'NPROCESSORS_ONLN)))

(define sysloadavg (c-extern 'sysloadavg (_array _double 3)))
(define-posix getloadavg
  (_fun #:save-errno 'posix
        [(_array _double 3) = sysloadavg]
        [_size = (array-length sysloadavg)]
        -> [$? : _int]
        -> (cond [($? . >= . 0) sysloadavg]
                 [else (raise-foreign-error 'getloadavg (saved-errno))])))

(define-posix sysconf
  (_fun #:save-errno 'posix
        _sysconf
        -> [$? : _long]
        -> (cond [($? . >= . 0) sysloadavg]
                 [else (raise-foreign-error 'sysconf (saved-errno))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module* typed/ffi typed/racket
  (provide (all-defined-out))

  (require (for-syntax racket/syntax))
  (require (for-syntax racket/string))

  (define-syntax (require/typed/provide/enums stx)
    (syntax-case stx []
      [(_ enums ...)
       (with-syntax ([([_etypes define/c->rackets define/racket->cs] ...)
                      (for/list ([elem (in-list (syntax->list #'(enums ...)))])
                        (syntax-case elem []
                          [enum (with-syntax ([_etype (format-id #'enum "_~a" (syntax-e #'enum))]
                                              [c->racket (format-id #'enum "~a-c->racket" (syntax-e #'enum))]
                                              [racket->c (format-id #'enum "~a-racket->c" (syntax-e #'enum))])
                                  #'[_etype
                                     (define (c->racket [c : Integer]) : Symbol (cast ((ctype-c->scheme _etype) c) Symbol))
                                     (define (racket->c [r : Symbol]) : Integer (cast ((ctype-scheme->c _etype) r) Integer))])]))])
         #'(begin (require/typed/provide (submod "..")
                                         [_etypes CType]
                                         ...)
                  define/c->rackets ...
                  define/racket->cs ...))]))

  (define-syntax (require/typed/provide/bitmasks stx)
    (syntax-case stx []
      [(_ bitmasks ...)
       (with-syntax ([([_btypes define/c->rackets define/racket->cs] ...)
                      (for/list ([elem (in-list (syntax->list #'(bitmasks ...)))])
                        (syntax-case elem []
                          [bitmask (with-syntax ([_btype (format-id #'bitmask "_~a" (syntax-e #'bitmask))]
                                                 [c->racket (format-id #'bitmask "~a-c->racket" (syntax-e #'bitmask))]
                                                 [racket->c (format-id #'bitmask "~a-racket->c" (syntax-e #'bitmask))])
                                     #'[_btype
                                        (define (c->racket [c : Natural]) : (Listof Symbol) (cast ((ctype-c->scheme _btype) c) (Listof Symbol)))
                                        (define (racket->c [r : (Listof Symbol)]) : Natural (cast ((ctype-scheme->c _btype) r) Natural))])]))])
         #'(begin (require/typed/provide (submod "..")
                                         [_btypes CType]
                                         ...)
                  define/c->rackets ...
                  define/racket->cs ...))]))
  
  (define-syntax (require/typed/provide/pointers stx)
    (syntax-case stx []
      [(_ Pointers/Opaques ...)
       (with-syntax ([([opaques ctypes ctype/nulls definetypes] ...)
                      (for/list ([Pointer/Opqaques (in-list (syntax->list #'(Pointers/Opaques ...)))])
                        (syntax-case Pointer/Opqaques []
                          [(Pointer pointer?)
                           (with-syntax ([Pointer/Null (format-id #'Pointer "~a/Null" (syntax-e #'Pointer))]
                                         [_ctype (format-id #'pointer? "_~a" (string-trim (symbol->string (syntax-e #'pointer?)) #px"\\?$"))]
                                         [_ctype/null (format-id #'pointer? "_~a/null" (string-trim (symbol->string (syntax-e #'pointer?)) #px"\\?$"))])
                             #'[[#:opaque Pointer pointer?]
                                [_ctype CType]
                                [_ctype/null CType]
                                (define-type Pointer/Null (Option Pointer))])]
                          [Pointer
                           (with-syntax ([pointer? (format-id #'Pointer "~a?" (string-downcase (symbol->string (syntax-e #'Pointer))))]
                                         [Pointer/Null (format-id #'Pointer "~a/Null" (syntax-e #'Pointer))]
                                         [_ctype (format-id #'pointer? "_~a" (string-downcase (symbol->string (syntax-e #'Pointer))))]
                                         [_ctype/null (format-id #'pointer? "_~a/null" (string-downcase (symbol->string (syntax-e #'Pointer))))])
                             #'[[#:opaque Pointer pointer?]
                                [_ctype CType]
                                [_ctype/null CType]
                                (define-type Pointer/Null (Option Pointer))])]))])
         #'(begin (require/typed/provide (submod "..")
                                         opaques ...
                                         ctypes ...
                                         ctype/nulls ...)
                  definetypes ...))]))
  
  (define-syntax (require/typed/provide/ctypes stx)
    (syntax-case stx []
      [(_ _ctype ...)
       #'(require/typed/provide (submod "..")
                                [_ctype CType]
                                ...)]))
  
  (require/typed/provide (submod "..")
                         [#:opaque CPointer/Null cpointer?]
                         [#:opaque CPointer/GCable cpointer-gcable?]
                         [#:opaque CType ctype?]
                         [#:opaque Array array?]
                         [ctype-basetype (-> CType (U False Symbol CType (Listof CType)))]
                         [ctype-c->scheme (-> CType (-> Any Any))]
                         [ctype-scheme->c (-> CType (-> Any Any))]
                         [array-type (-> Array CType)]
                         [array-length (-> Array Index)]
                         [in-array (->* [Array] [Positive-Index (Option Positive-Index) Positive-Integer] (Sequenceof Any))]
                         [array-ref (-> Array Index * Any)]
                         [array-set! (case-> [Array Index Any -> Void]
                                             [Array Index Index Any -> Void]
                                             [Array Index Index Index Any -> Void])])

  (require/typed/provide/ctypes _uintmax _byte _sint32 _string*/utf-8 _void
                                _int8 _uint8 _int16 _uint16 _int32 _uint32 _int64 _uint64
                                _fixint _ufixint _fixnum _ufixnum _float _double _longdouble
                                _double* _bool _stdbool _string/ucs-4 _string/utf-16 _path
                                _symbol _pointer _gcpointer _scheme _fpointer _racket _ssize
                                _size _uword _word _sbyte _string*/latin-1 _bytes/eof _file
                                _intmax _ptrdiff _sintptr _intptr _sllong _ullong _llong
                                _slong _ulong _long _sint _uint _int _sshort _ushort _short
                                _ubyte _sint64 _sint16 _sint8 _string*/locale _string/latin-1
                                _string/locale _string/utf-8 _uintptr _sword)
  
  (require/typed/provide/enums facility severity)
  (require/typed/provide/bitmasks logflags)
  
  (require/typed/provide (submod "..")
                         [#:opaque CPointer cvoid*?]
                         [#:struct (exn:foreign exn:fail) ([errno : Integer] [strerror : (-> Integer String)])]
                         [#:struct (exn:break:signal exn:break) ([signo : Positive-Integer])]
                         [c-extern (-> (U String Bytes Symbol) CType Any)]
                         [raise-foreign-error (-> Any Natural [#:strerror (-> Integer String)] exn:foreign)]
                         [raise-signal-error (-> (U Symbol Positive-Integer) exn:break:signal)])
  
  (require/typed/provide (submod "..")
                         [strerror (-> Natural String)]
                         [gai_strerror (-> Positive-Integer String)]
                         [strsignal (-> Positive-Integer String)]
                         [getppid (-> Natural)]
                         [getpid (-> Natural)]
                         [getuid (-> Natural)]
                         [getgid (-> Natural)]
                         [geteuid (-> Natural)]
                         [getegid (-> Natural)]
                         [seteuid (-> Natural Void)]
                         [setegid (-> Natural Void)]
                         [fetch_tamer_ids (-> Bytes (Values Natural Natural))]
                         [fetch_tamer_name (-> Natural Bytes)]
                         [fetch_tamer_group (-> Natural Bytes)]
                         [openlog (-> String (Listof Symbol) Symbol Void)]
                         [syslog (-> Symbol String Void)]
                         [setlogmask_one (-> Symbol Void)]
                         [setlogmask_upto (-> Symbol Void)]
                         [closelog (-> Void)]
                         [sysloadavg Array]
                         [getloadavg (-> Array)]))
