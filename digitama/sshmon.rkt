#lang typed/racket

(require "syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4250, The Secure Shell Protocol Assigned Numbers              ;;;
;;; https://tools.ietf.org/html/rfc4251, The Secure Shell Protocol Architecture                  ;;;
;;; https://tools.ietf.org/html/rfc4252, The Secure Shell Authentication Protocol                ;;;
;;; https://tools.ietf.org/html/rfc4253, The Secure Shell Transport Layer Protocol               ;;;
;;; https://tools.ietf.org/html/rfc4254, The Secure Shell Connection Protocol                    ;;;
;;;                                                                                              ;;;
;;; https://tools.ietf.org/html/rfc6668, The Secure Shell Transport Layer Protocol with SHA-2    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(define ssh-custodian : Custodian (make-custodian))
(define on-error-do : (Parameter (-> Any)) (make-parameter void))

(define-syntax (debug-and-reraise stx)
  (syntax-case stx []
    [(_ st-id f-id event)
     #'(lambda [[src : exn]]
         (let ([bugmsg (format "~a: ~a: ~a" f-id event (exn-message src))])
           ((on-error-do))
           (log-debug bugmsg)
           (raise (struct-copy st-id src [message #:parent exn bugmsg]))))]))

(define-type/enum ssh-protocols : SSH-Protocol '|2.0|)

; <http://tools.ietf.org/html/rfc4250#section-4.11>
(define-type/enum ssh-algorithm/encryption : SSH-Algorithm/Encryption
  ; <http://tools.ietf.org/html/rfc4253#section-6.3>
  '3des-cbc 'blowfish-cbc 'twofish256-cbc 'twofish-cbc 'twofish192-cbc 'twofish128-cbc
  'aes256-cbc 'aes192-cbc 'aes128-cbc 'serpent256-cbc 'serpent192-cbc 'serpent128-cbc
  'arcfour 'idea-cbc 'cast128-cbc 'none 'des-cbc)

(define-type/enum ssh-algorithm/mac : SSH-Algorithm/MAC
  ; <http://tools.ietf.org/html/rfc4253#section-6.4>
  'hmac-sha1 'hmac-sha1-96 'hmac-md5 'hmac-md5-96 'none)

(define-type/enum ssh-algorithm/publickey : SSH-Algorithm/Publickey
  ; <http://tools.ietf.org/html/rfc4253#section-6.6>
  'ssh-dss 'ssh-rsa 'pgp-sign-rsa 'pgp-sign-dss)

(define-type/enum ssh-algorithm/compression : SSH-Algorithm/Compression
  ; <http://tools.ietf.org/html/rfc4253#section-6.2>
  'none 'zlib)

;;; Message Constants <http://tools.ietf.org/html/rfc4250#section-4.1>
(defconsts:s/_/-/g
  [SSH_MSG_DISCONNECT                       1     [SSH-TRANS]]
  [SSH_MSG_IGNORE                           2     [SSH-TRANS]]
  [SSH_MSG_UNIMPLEMENTED                    3     [SSH-TRANS]]
  [SSH_MSG_DEBUG                            4     [SSH-TRANS]]
  [SSH_MSG_SERVICE_REQUEST                  5     [SSH-TRANS]]
  [SSH_MSG_SERVICE_ACCEPT                   6     [SSH-TRANS]]
  [SSH_MSG_KEXINIT                         20     [SSH-TRANS]]
  [SSH_MSG_NEWKEYS                         21     [SSH-TRANS]]
  [SSH_MSG_USERAUTH_REQUEST                50     [SSH-USERAUTH]]
  [SSH_MSG_USERAUTH_FAILURE                51     [SSH-USERAUTH]]
  [SSH_MSG_USERAUTH_SUCCESS                52     [SSH-USERAUTH]]
  [SSH_MSG_USERAUTH_BANNER                 53     [SSH-USERAUTH]]
  [SSH_MSG_GLOBAL_REQUEST                  80     [SSH-CONNECT]]
  [SSH_MSG_REQUEST_SUCCESS                 81     [SSH-CONNECT]]
  [SSH_MSG_REQUEST_FAILURE                 82     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_OPEN                    90     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_OPEN_CONFIRMATION       91     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_OPEN_FAILURE            92     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_WINDOW_ADJUST           93     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_DATA                    94     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_EXTENDED_DATA           95     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_EOF                     96     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_CLOSE                   97     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_REQUEST                 98     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_SUCCESS                 99     [SSH-CONNECT]]
  [SSH_MSG_CHANNEL_FAILURE                100     [SSH-CONNECT]])

(defconsts:s/_/-/g
  [SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT          1]
  [SSH_DISCONNECT_PROTOCOL_ERROR                       2]
  [SSH_DISCONNECT_KEY_EXCHANGE_FAILED                  3]
  [SSH_DISCONNECT_RESERVED                             4]
  [SSH_DISCONNECT_MAC_ERROR                            5]
  [SSH_DISCONNECT_COMPRESSION_ERROR                    6]
  [SSH_DISCONNECT_SERVICE_NOT_AVAILABLE                7]
  [SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED       8]
  [SSH_DISCONNECT_HOST_KEY_NOT_VERIFIABLE              9]
  [SSH_DISCONNECT_CONNECTION_LOST                     10]
  [SSH_DISCONNECT_BY_APPLICATION                      11]
  [SSH_DISCONNECT_TOO_MANY_CONNECTIONS                12]
  [SSH_DISCONNECT_AUTH_CANCELLED_BY_USER              13]
  [SSH_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE      14]
  [SSH_DISCONNECT_ILLEGAL_USER_NAME                   15])

;;; SSH Datatype Representations <http://tools.ietf.org/html/rfc4251#section-5>
(define ssh-boolean->bytes : (-> Any Bytes)
  (lambda [bool]
    (if bool (bytes 1) (bytes 0))))

(define ssh-bytes->boolean : (-> Bytes [#:offset Index] Boolean)
  (lambda [bbool #:offset [offset 0]]
    (false? (zero? (bytes-ref bbool offset)))))

(define ssh-uint32->bytes : (-> Nonnegative-Fixnum Bytes)
  (lambda [u32]
    (integer->integer-bytes u32 4 #false #true)))

(define ssh-bytes->uint32 : (-> Bytes [#:offset Index] Nonnegative-Fixnum)
  (lambda [bint #:offset [offset 0]]
    (cast (integer-bytes->integer bint #false #true offset (+ offset 4)) Nonnegative-Fixnum)))

(define ssh-uint64->bytes : (-> Nonnegative-Integer Bytes)
  (lambda [u64]
    (integer->integer-bytes u64 8 #false #true)))

(define ssh-bytes->uint64 : (-> Bytes [#:offset Index] Nonnegative-Integer)
  (lambda [bint #:offset [offset 0]]
    (cast (integer-bytes->integer bint #false #true offset (+ offset 8)) Nonnegative-Fixnum)))

(define ssh-string->bytes : (-> String Bytes)
  (lambda [utf8]
    (bytes-append (ssh-uint32->bytes (string-utf-8-length utf8))
                  (string->bytes/utf-8 utf8))))

(define ssh-bytes->string : (-> Bytes [#:offset Index] String)
  (lambda [butf8 #:offset [offset 0]]
    (bytes->string/utf-8 butf8 #false (+ offset 4) (+ offset 4 (ssh-bytes->uint32 butf8 #:offset offset)))))

(define ssh-mpint->bytes : (-> Integer Bytes)
  (lambda [mpi]
    (define ceiling : Integer (exact-ceiling (/ (integer-length mpi) 8)))
    (let mpint->bytes : Bytes ([blist : (Listof Byte) null])
      (define n : Index (length blist))
      (cond [(< n ceiling) (mpint->bytes (cons (bitwise-and (arithmetic-shift mpi (* n -8)) #xFF) blist))]
            [(and (positive? mpi) (= (car blist) #b10000000)) (mpint->bytes (cons #x00 blist))]
            [(and (negative? mpi) (false? (bitwise-bit-set? (car blist) 7))) (mpint->bytes (cons #xFF blist))]
            [else (bytes-append (ssh-uint32->bytes n) (list->bytes blist))]))))

(define ssh-bytes->mpint : (-> Bytes [#:offset Index] Integer)
  (lambda [bmpi #:offset [offset 0]]
    (define len : Integer (ssh-bytes->uint32 bmpi #:offset offset))
    (cond [(zero? len) 0]
          [else (let bytes->mpint ([idx : Integer (+ offset 4 1)]
                                   [mpint : Integer (let ([mpi0 : Byte (bytes-ref bmpi (+ offset 4))])
                                                      (if (> mpi0 #b01111111) (- mpi0 #x100) mpi0))])
                  (cond [(zero? (- idx len offset 4)) mpint]
                        [else (bytes->mpint (add1 idx)
                                            (bitwise-ior (arithmetic-shift mpint 8)
                                                         (bytes-ref bmpi idx)))]))])))

(define ssh-namelist->bytes : (-> (Listof Symbol) Bytes)
  (lambda [names]
    (ssh-string->bytes (string-join (map symbol->string names) ","))))

(define ssh-bytes->namelist : (-> Bytes [#:offset Index] (Listof Symbol))
  (lambda [bascii #:offset [offset 0]]
    (map string->symbol (string-split (ssh-bytes->string bascii #:offset offset) ","))))
;;; End SSH Datatype

(define-type SSH-Session<%>
  (Class (init [host String]
               [port Integer #:optional]
               [protocol SSH-Protocol #:optional]
               [enable-break? Boolean #:optional]
               [on-debug (-> String String Any Any) #:optional])
         [collapse (-> Void)]))

(define ssh-session% : SSH-Session<%>
  (class object% (super-new)
    (init host)
    (init [port 22]
          [protocol '|2.0|]
          [enable-break? #true]
          [on-debug void])
    
    (define logger : Logger (make-logger (string->symbol (format "#<~a:~a:~a>" (object-name this) host port)) #false))
    (define logging : Thread
      (parameterize ([current-custodian ssh-custodian])
        (thread (thunk (let sync-handle-loop ([event (make-log-receiver logger 'debug)])
                         (match (sync event)
                           [(vector _ _ 'collapse _) (void 'job-done)]
                           [(vector always-debug message attachment _)
                            (void (on-debug host message attachment))
                            (sync-handle-loop event)]))))))
    
    (define hostname : String host)
    (define portno : Integer port)
    (define version : SSH-Protocol protocol)
    (define banner : String (format "SSH-~a-WarGreySSH_0.6 Racket" version))

    ;;; WARNING: (define-values) and (match-define) annoy typed racket here
    ;; initializing and handshaking <http://tools.ietf.org/html/rfc4253#section-4.2>
    (define /dev/sshio : (Vector (Option Input-Port) (Option Output-Port)) (vector #false #false))
    (exchange-identification enable-break?)
    (exchange-key)
    ;;; End WARNING
    
    (define/public (collapse)
      (for ([sshio (in-vector /dev/sshio)])
        (when (tcp-port? sshio) (tcp-abandon-port sshio)))
      (log-message logger 'fatal "don't panic" 'collapse)
      (thread-wait logging))

    (define/private (exchange-identification [enable-break? : Boolean]) : Void
      (parameterize ([current-logger logger]
                     [current-custodian (make-custodian ssh-custodian)]
                     [on-error-do (thunk (custodian-shutdown-all (current-custodian)))])
        (define-values (sshin sshout) ((if enable-break? tcp-connect/enable-break tcp-connect) hostname portno))
        (define rfc-banner : String (~a banner #:max-width 253))
        
        (with-handlers ([exn:fail? (debug-and-reraise exn:fail 'ssh-handshake "failed sending identification")])
          ; NOTE: RFC does not define the order who initaites the exchange process,
          ;       nonetheless, the client sends first is always not bad.
          (fprintf sshout "~a~a~a" rfc-banner #\return #\linefeed)
          (log-debug "sending identification: ~a" rfc-banner))
        
        (with-handlers ([exn:fail? (debug-and-reraise exn:fail 'ssh-handshake "failed getting identification")])
          (unless (sync/timeout/enable-break 1.618 sshin)
            (error "timed out")))
        
        (let/cc break : Any
          (for ([line (in-port (lambda [[in : Input-Port]] (read-line in 'linefeed)) sshin)])
            (cond [(false? (and (string? line) (regexp-match? #px"^SSH-" line))) (log-debug (string-trim line))] ; TODO: filter terminal control chars
                  [else (match-let ([(list-rest _ protoversion softwareversion comments) (string-split line #px"-|\\s")])
                          (unless (member protoversion (list "1.99" "2.0"))
                            ; NOTE: if server is older then client, then client should close connection
                            ;       and reconnect with the old protocol. It seems that the rules checking
                            ;       compatibility mode is not guaranteed.
                            (with-handlers ([exn:fail? (debug-and-reraise exn:fail:unsupported 'ssh-handshake "unknown SSH protocol")])
                              (raise (make-exn:fail:unsupported (string-trim line) (current-continuation-marks)))))
                          (log-debug "received identification: ~a" (string-trim line))
                          (break 'I-do-not-see-servers-that-send-data-after-this-process))])))
        
        (vector-set! /dev/sshio 0 sshin)
        (vector-set! /dev/sshio 1 sshout)))

    (define/private (exchange-key) : Bytes
      #"here")))

(module* main racket
  (require (submod ".."))
  
  (for ([host (in-list (list "172.16.1.9" "172.16.1.6" "gyoudmon.org"))])
    (with-handlers ([exn? (curry eprintf "~a")])
      (define ssh (new ssh-session% [host host] [port 22] [on-debug (lambda [topic message attachment] (displayln message))]))
      (send ssh collapse))))
