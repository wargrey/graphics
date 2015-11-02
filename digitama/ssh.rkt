#lang typed/racket

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

(require typed/openssl/md5)

(require "syntax.rkt")

(require/typed racket/base
               [string-port? (-> Port Boolean)]
               [port-progress-evt (-> Input-Port (Option (-> (Evtof Input-Port))))])

(define ssh-custodian : Custodian (make-custodian))

(struct exn:ssh exn:fail ())
(struct exn:ssh:eof exn:ssh ([reason : SSH-Disconnection-Reason]))
(struct exn:ssh:again exn:ssh ())

(define-type/enum ssh-protocols : SSH-Protocol 2.0)

;;; Message Constants <http://tools.ietf.org/html/rfc4250#section-4.1>
(define-type/consts sm : SSH-Message-Type of Byte 
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

(define-type/consts sd : SSH-Disconnection-Reason of Nonnegative-Fixnum 
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
(define-type SSH-DataType (U Boolean Byte Bytes uint32 uint64 String Integer (Listof Symbol)))

(struct uint32 ([ref : Nonnegative-Fixnum]) #:mutable #:prefab)
(struct uint64 ([ref : Natural]) #:mutable #:prefab)
(struct packet ([type : SSH-Message-Type] [payloads : (Listof SSH-DataType)]))

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

; <http://tools.ietf.org/html/rfc4250#section-4.11>
(define-type/enum ssh-algorithms/cipher : SSH-Algorithm/Cipher ; <http://tools.ietf.org/html/rfc4253#section-6.3>
  [3des-cbc         REQUIRED          three-key 3DES in CBC mode]
  [blowfish-cbc     OPTIONAL          Blowfish in CBC mode]
  [twofish256-cbc   OPTIONAL          Twofish in CBC mode with a 256-bit key]
  [twofish-cbc      OPTIONAL          alias for twofish256-cbc]
  [twofish192-cbc   OPTIONAL          Twofish with a 192-bit key]
  [twofish128-cbc   OPTIONAL          Twofish with a 128-bit key]
  [aes256-cbc       OPTIONAL          AES in CBC mode with a 256-bit key]
  [aes192-cbc       OPTIONAL          AES with a 192-bit key]
  [aes128-cbc       RECOMMENDED       AES with a 128-bit key]
  [serpent256-cbc   OPTIONAL          Serpent in CBC mode with a 256-bit key]
  [serpent192-cbc   OPTIONAL          Serpent with a 192-bit key]
  [serpent128-cbc   OPTIONAL          Serpent with a 128-bit key]
  [arcfour          OPTIONAL          the ARCFOUR stream cipher with a 128-bit key]
  [idea-cbc         OPTIONAL          IDEA in CBC mode]
  [cast128-cbc      OPTIONAL          CAST-128 in CBC mode]
  [none             OPTIONAL          no encryption])

(define-type/enum ssh-algorithms/mac : SSH-Algorithm/MAC ; <http://tools.ietf.org/html/rfc4253#section-6.4>
  [hmac-sha1    REQUIRED        HMAC-SHA1 (digest length = key length = 20)]
  [hmac-sha1-96 RECOMMENDED     first 96 bits of HMAC-SHA1 (digest length = 12, key length = 20)]
  [hmac-md5     OPTIONAL        HMAC-MD5 (digest length = key length = 16)]
  [hmac-md5-96  OPTIONAL        first 96 bits of HMAC-MD5 (digest length = 12, key length = 16)]
  [none         OPTIONAL        no MAC]

  ; <http://tools.ietf.org/html/rfc6668#section-2>
  [hmac-sha2-256     RECOMMENDED   HMAC-SHA2-256 (digest length = 32 bytes key length = 32 bytes)]
  [hmac-sha2-512     OPTIONAL      HMAC-SHA2-512 (digest length = 64 bytes key length = 64 bytes)])

(define-type/enum ssh-algorithms/publickey : SSH-Algorithm/Publickey ; <http://tools.ietf.org/html/rfc4253#section-6.6>
  [ssh-dss           REQUIRED     sign   Raw DSS Key]
  [ssh-rsa           RECOMMENDED  sign   Raw RSA Key]
  [pgp-sign-rsa      OPTIONAL     sign   OpenPGP certificates (RSA key)]
  [pgp-sign-dss      OPTIONAL     sign   OpenPGP certificates (DSS key)])

(define-type/enum ssh-algorithms/compression : SSH-Algorithm/Compression ; <http://tools.ietf.org/html/rfc4253#section-6.2>
  [none     REQUIRED        no compression]
  [zlib     OPTIONAL        ZLIB (LZ77) compression])

(define-type/enum ssh-algorithms/kex : SSH-Algorithm/Kex ; <http://tools.ietf.org/html/rfc4253#section-8>
  [diffie-hellman-group1-sha1 REQUIRED]
  [diffie-hellman-group14-sha1 REQUIRED])

(define-type SSH-Session<%>
  (Class (init [host String]
               [port Integer #:optional]
               [protocol SSH-Protocol #:optional]
               [breakable? Boolean #:optional]
               [on-debug (-> Symbol String Any Symbol Any) #:optional])
         [session-name (-> Symbol)]
         [collapse (->* () (String SSH-Disconnection-Reason) Void)]))

(define ssh-session% : SSH-Session<%>
  (class object% (super-new)
    (init host)
    (init [port 22]
          [protocol 2.0]
          [breakable? #true]
          [on-debug void])

    (define watchcat : Custodian (make-custodian ssh-custodian))

    (define topic : Symbol (string->symbol (format "#<~a:~a:~a>" (object-name this) host port)))
    (define sshlog : Logger (make-logger topic #false))

    (define log-ssh : (-> Log-Level String [#:extra Any] Any * Void)
      (lambda [level #:extra [extra (current-continuation-marks)] maybe-fmt . argl]
        (log-message sshlog level #false (if (null? argl) maybe-fmt (apply format maybe-fmt argl)) extra)))
    
    (define logging : Thread
      (parameterize ([current-custodian ssh-custodian])
        (thread (thunk (let sync-match-trigger-loop ([event (make-log-receiver sshlog 'debug)])
                         (match (sync/timeout/enable-break 0.1 event)
                           [(? false?) (sync-match-trigger-loop event)]
                           [(vector level message (and extra (vector 'SSH_MSG_DISCONNECT _)) _)
                            (on-debug level message extra topic)]
                           [(vector level message extra _)
                            (void (on-debug level message extra topic)
                                  (sync-match-trigger-loop event))]))))))

    (define hostname : String host)
    (define portno : Integer port)
    (define sshversion : SSH-Protocol protocol)
    (define banner : String (format "SSH-~a-WarGrey_SSHmon_0.6 Racket-~a" sshversion (version)))
    (define/public session-name (lambda [] topic))
    
    ;;; WARNING: (define-values) and (match-define) annoy typed racket here
    (define /dev/sshin  : Input-Port  (open-input-bytes #""))
    (define /dev/sshout : Output-Port (open-output-nowhere))
    ;;; End WARNING

    (define cipher-blocksize : Byte 0)
    (define message-authsize : Byte 0)
    (define compression : SSH-Algorithm/Compression 'none)

    (with-handlers ([exn:ssh:eof? (lambda [[e : exn:ssh:eof]] (raise (and (collapse (exn-message e) (exn:ssh:eof-reason e)) e)))]
                    [exn? (lambda [[e : exn]] (raise (and (collapse (exn-message e)) e)))])
      (parameterize ([current-custodian watchcat])
        ;; initializing and handshaking
        (define-values (tcpin tcpout) ((if breakable? tcp-connect/enable-break tcp-connect) hostname portno))
        (define rfc-banner : String (~a banner #:max-width 253))

        ; <http://tools.ietf.org/html/rfc4253#section-4.2>
        (with-handlers ([exn:fail? (rethrow [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "failed to send identification")])
          ; NOTE: RFC does not define the order who initaites the exchange process,
          ;       nonetheless, the client sends first is always not bad.
          (fprintf tcpout "~a~a~a" rfc-banner #\return #\linefeed)
          (flush-output tcpout)
          (log-ssh 'debug "sent identification: ~a" rfc-banner))
      
        (unless (sync/timeout/enable-break 1.618 tcpin)
          (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "fail to get identification: timed out"))
        
        (let handshake : Void ()
          (define line : (U String EOF) (read-line tcpin 'linefeed))
          (cond [(eof-object? line)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "did not receive identification string from ~a" hostname)]
                [(false? (regexp-match? #px"^SSH-" line))
                 ; TODO: RFC says control chars should be filtered
                 (log-ssh 'debug (string-trim line))
                 (handshake)]
                [else (match-let ([(list-rest _ protoversion softwareversion comments) (string-split line #px"-|\\s")])
                        (set!-values (/dev/sshin /dev/sshout) (make-/dev/sshio tcpin tcpout))
                        (unless (member protoversion (list "1.99" "2.0"))
                          ; NOTE: if server is older then client, then client should close connection
                          ;       and reconnect with the old protocol. It seems that the rules checking
                          ;       compatibility mode is not guaranteed.
                          (throw [exn:ssh:eof 'SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED] "unsupported protocol: ~a" (string-trim line)))
                        (log-ssh 'debug "received identification: ~a" (string-trim line))
                        (exchange-key))]))))
    
    (define/public (collapse [description "job done"] [reason 'SSH_DISCONNECT_BY_APPLICATION])
      (unless (or (string-port? /dev/sshin) (port-closed? /dev/sshout))
        (with-handlers ([exn? (lambda [[e : exn]] (void 'already 'logged 'by 'transport-send))])
          (transport-send 'SSH_MSG_DISCONNECT (uint32 ($#sd reason)) description ""))
        (close-input-port /dev/sshin)
        (close-output-port /dev/sshout))
      (log-ssh 'debug description #:extra (vector 'SSH_MSG_DISCONNECT reason))
      (thread-wait logging)
      (custodian-shutdown-all watchcat))
    
    (define/private (make-/dev/sshio [tcpin : Input-Port] [tcpout : Output-Port]) : (Values Input-Port Output-Port)
      (define largest-packet-buffer : Bytes (make-bytes 35000))

      ; http://tools.ietf.org/html/rfc4253#section-6
      #| uint32    packet_length  (the next 3 fields)               -
         byte      padding_length (in the range of [4, 255])         \ the size of these 4 fields should be multiple of
         byte[n1]  payload; n1 = packet_length - padding_length - 1  / 8 or cipher-blocksize whichever is larger
         byte[n2]  random padding; n2 = padding_length              -
         byte[m]   mac (Message Authentication Code - MAC); m = mac_length |#
      (define transport-recv-packet : (-> Bytes (U Nonnegative-Integer EOF Procedure))
        (lambda [userland] ;;; WARNING: This is not thread safe!
          ; SSH will never return EOF in normal case, API clients should catch the exception on their own.
          (with-handlers ([(curry eq? 'exn:ssh:again) (const 0)]) 
            (define packet-length : Natural
              (let ([read (read-bytes-avail!* largest-packet-buffer tcpin 0 (max cipher-blocksize 4))])
                (cond [(eof-object? read) (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "connection closed by ~a" hostname)]
                      [(procedure? read) (throw exn:ssh "special value cannot be here!")]
                      [(zero? read) (throw exn:ssh:again "no message is on boat!")]
                      [else (ssh-bytes->uint32 largest-packet-buffer)])))
              packet-length)))

      (define transport-send-packet : (-> Any Boolean Boolean True #| Details see the Racket Reference (make-output-port) |#)
        (lambda [raw always-nonblock-by-me-due-to-disabled-break break-always-disabled-by-racket]
          (define-values (id payload-raw) 
            (if (false? (packet? raw))
                (values 'SSH_MSG_IGNORE
                        (bytes-append (bytes ($#sm 'SSH_MSG_IGNORE))
                                      (string->bytes/utf-8 (~s raw))))
                (values (packet-type raw)
                        (for/fold ([payload : Bytes (bytes ($#sm (packet-type raw)))])
                                  ([content : SSH-DataType (in-list (packet-payloads raw))])
                          (bytes-append payload (match content
                                                  [(? byte? b) (bytes b)]
                                                  [(? bytes? bstr) bstr]
                                                  [(? boolean? b) (ssh-boolean->bytes b)]
                                                  [(uint32 fx) (ssh-uint32->bytes fx)]
                                                  [(uint64 n) (ssh-uint64->bytes n)]
                                                  [(? string? str) (ssh-string->bytes str)]
                                                  [(? exact-integer? mpi) (ssh-mpint->bytes mpi)]
                                                  [(? list?) (ssh-namelist->bytes (cast content (Listof Symbol)))]))))))
        
          (when (> (bytes-length payload-raw) 32768)
            (throw exn:ssh "packet is too large to send."))
          
          (define payload : Bytes payload-raw)
          ; TODO: compress
          (define payload-length : Integer (bytes-length payload))

          (define-values (packet-length padding-length)
            (let* ([idsize : Byte (max 8 cipher-blocksize)]
                   [packet-draft : Integer (+ 4 1 payload-length)]
                   [padding-draft : Integer (- idsize (remainder packet-draft idsize))]
                   [padding-draft : Integer (if (< padding-draft 4) (+ padding-draft idsize) padding-draft)]
                   [capacity : Integer (quotient (- #xFF padding-draft) (add1 idsize))] ; for thwarting traffic analysis
                   [random-length : Integer (+ padding-draft (if (< capacity 1) 0 (* idsize (random capacity))))])
              (values (cast (+ packet-draft random-length -4) Nonnegative-Fixnum) random-length)))

          (define random-padding : Bytes (make-bytes padding-length))
          (for ([i (in-range padding-length)])
            (bytes-set! random-padding i (bytes-ref payload (random payload-length))))
          
          (define mac : Bytes (make-bytes message-authsize))

          (with-handlers ([exn? (rethrow exn:ssh "failed to send packet ~a" id)])
            (define packet : Bytes (bytes-append (ssh-uint32->bytes packet-length) (bytes padding-length) payload random-padding mac))
            (define total : Index (bytes-length packet))
            (log-ssh 'debug "sending ~a of ~a bytes (+ 4 1 ~a ~a ~a)" id total payload-length padding-length message-authsize)
            (define sent : (Option Index) (write-bytes-avail* packet tcpout 0 total))
            (cond [(false? sent) (throw exn:ssh:again "network is busy")]
                  [else (log-ssh 'info #:extra (vector id sent total)
                                 "sent ~a: ~a bytes, ~a% done" id sent (~r (* 100 (/ sent total)) #:precision '(= 2)))]))
          #true))

      (values (make-input-port (string->symbol (format "ssh:~a" hostname))
                               transport-recv-packet #false
                               (thunk (tcp-abandon-port tcpin)))
              (make-output-port (string->symbol (format "ssh:~a" hostname))
                                (cast tcpout (Evtof Output-Port))
                                tcpout (thunk (tcp-abandon-port tcpout))
                                transport-send-packet)))

    (define/private (exchange-key) : Void
      ; <http://tools.ietf.org/html/rfc4253#section-7.1>
      ;(unless (input-port? (sync/timeout 0 /dev/sshin))
      ; (log-ssh 'debug "we send SSH_MSG_KEXINIT first"))
        
      (transport-send 'SSH_MSG_KEXINIT
                      (call-with-input-string (number->string (current-inexact-milliseconds)) md5-bytes) ; cookie
                      ssh-algorithms/kex
                      ssh-algorithms/publickey
                      ssh-algorithms/cipher #| local |# ssh-algorithms/cipher #| remote |#
                      ssh-algorithms/mac #| local |# ssh-algorithms/mac #| remote |#
                      ssh-algorithms/compression #| local |# ssh-algorithms/compression #| remote |#
                      null #| language local |# null #| language remote |#
                      #false #| whether a guessed key exchange packet follows |#
                      (uint32 0) #| reserved, always 0 |#))
      
    (define/private (transport-send [id : SSH-Message-Type] . [payloads : SSH-DataType *]) : Void
      (void (write-special (packet id payloads) /dev/sshout)))))

(module* main racket
  (require (submod ".."))

  (define show-debuginfo
    (lambda [level message extra session]
      (if (exn? extra)
          (fprintf (current-error-port) "[~a] ~a: ~a~n" (object-name extra) session (exn-message extra))
          (fprintf (current-output-port) "[~a] ~a: ~a~n" level session message))))
  
  (for ([host (in-list (list "localhost" "gyoudmon.org"))])
    (with-handlers ([exn? void])
      (define ssh (new ssh-session% [host host] [port 22] [on-debug show-debuginfo]))
      (send ssh collapse "demonstration done" 'SSH_DISCONNECT_BY_APPLICATION))))
