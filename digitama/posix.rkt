#lang at-exp racket

(provide (except-out (all-defined-out) define-posix define-digitama))
(provide (all-from-out "ffi.rkt"))

@require{ffi.rkt}

(struct exn:foreign exn:fail (errno strerror))
(struct exn:break:signal exn:break (signo))

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
(define-ffi-definer define-posix (ffi-lib #false)
  #:default-make-fail make-not-available)

(define-ffi-definer define-digitama (digimon-ffi-lib "posix" #:global? #true #:on-fail (thunk (ffi-lib #false)))
  #:default-make-fail make-not-available)

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
  (provide (all-from-out (submod "ffi.rkt" typed)))

  (require (submod "ffi.rkt" typed))
  
  (require/typed/provide/enums facility severity)
  (require/typed/provide/bitmasks logflags)
  
  (require/typed/provide (submod "..")
                         [#:struct (exn:foreign exn:fail) ([errno : Integer] [strerror : (-> Integer String)])]
                         [#:struct (exn:break:signal exn:break) ([signo : Positive-Integer])]
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
