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

(define ssh-custodian : Custodian (make-custodian))

(struct exn:ssh exn:fail ())
(struct exn:ssh:unsupported exn:ssh ())

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

(define-type/consts sd : SSH-Broken-Reason of Byte 
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
               [enable-break? Boolean #:optional]
               [on-debug (-> Symbol String Any Any) #:optional])
         [session-name (-> Symbol)]
         [collapse (-> Void)]))

(define ssh-session% : SSH-Session<%>
  (class object% (super-new)
    (init host)
    (init [port 22]
          [protocol 2.0]
          [enable-break? #true]
          [on-debug void])

    (define portspace : Custodian (make-custodian ssh-custodian))

    (define topic : Symbol (string->symbol (format "#<~a:~a:~a>" (object-name this) host port)))
    (define logger : Logger (make-logger topic #false))
    (define logging : Thread
      (parameterize ([current-custodian ssh-custodian])
        (thread (thunk (let sync-handle-loop ([event (make-log-receiver logger 'debug)])
                         (match (sync event)
                           [(vector _ _ 'collapse _) 'job-done]
                           [(vector always-debug message attachment topic)
                            (void (on-debug (cast topic Symbol) message attachment)
                                  (sync-handle-loop event))]))))))
    
    (define hostname : String host)
    (define portno : Integer port)
    (define version : SSH-Protocol protocol)
    (define banner : String (format "SSH-~a-WarGreySSH_0.6 Racket" version))

    ;;; WARNING: (define-values) and (match-define) annoy typed racket here
    (define /dev/sshio : (Vector (Option Input-Port) (Option Output-Port)) (vector #false #false))
    (define cipher-blocksize : (Boxof (Option Byte)) (box #false))
    (define message-authsize : (Boxof (Option Byte)) (box #false))
    (define compression : (Boxof SSH-Algorithm/Compression) (box 'none))

    (parameterize ([current-logger logger]
                   [current-custodian portspace]
                   [on-error-do (thunk (custodian-shutdown-all portspace))])
      ;; initializing and handshaking
      (exchange-identification enable-break?)
      (exchange-key))
    ;;; End WARNING

    (define/public (session-name)
      topic)
    
    (define/public (collapse)
      (for ([sshio (in-vector /dev/sshio)])
        (when (tcp-port? sshio) (tcp-abandon-port sshio)))
      (log-message logger 'fatal "don't panic" 'collapse)
      (thread-wait logging))

    (define/private (transport-send [id : SSH-Message-Type] . [payloads : Bytes *]) : Void
      ; http://tools.ietf.org/html/rfc4253#section-6
      #|  uint32    packet_length  (the next 3 fields)               -
          byte      padding_length (in the range of [4, 255])         \ the size of these 4 fields should be multiple of
          byte[n1]  payload; n1 = packet_length - padding_length - 1  / 8 or cipher-blocksize whichever is larger
          byte[n2]  random padding; n2 = padding_length              -
          byte[m]   mac (Message Authentication Code - MAC); m = mac_length |#
      (define sshout : Output-Port (cast (vector-ref /dev/sshio 1) Output-Port))

      (define payload-length : Integer (foldl + 1 #| the message id takes one byte |# (map bytes-length payloads)))
      (when (> payload-length 32768)
        (throw exn:ssh:unsupported 'transport-send "packet is too large to send."))
      
      (define payload : Bytes (foldl bytes-append (bytes ($#sm id)) payloads))
      (define-values (packet-length padding-length)
        (let* ([idsize : Byte (max 8 (or (unbox cipher-blocksize) 0))]
               [packet-draft : Integer (+ 4 1 payload-length)]
               [padding-draft : Integer (- idsize (remainder packet-draft idsize))]
               [padding-draft : Integer (if (< padding-draft 4) (+ padding-draft idsize) padding-draft)]
               [capacity : Integer (quotient (- #xFF padding-draft) (add1 idsize))] ; for thwarting traffic analysis
               [random-length : Integer (+ padding-draft (if (< capacity 1) 0 (* idsize (random capacity))))])
          (values (cast (+ packet-draft random-length -4) Nonnegative-Fixnum) random-length)))
      
      (define random-padding : Bytes (make-bytes padding-length))
      (for ([i (in-range padding-length)])
        (bytes-set! random-padding i (bytes-ref payload (random payload-length))))

      (define mac-length : Integer (or (unbox message-authsize) 0))
      (define mac : Bytes (make-bytes mac-length))

      (with-handlers ([exn? (rethrow exn:ssh 'transport-send "failed send packet: ~a" id)])
        (define packet : Bytes (bytes-append (ssh-uint32->bytes packet-length) (bytes padding-length) payload random-padding mac))
        (define total : Index (bytes-length packet))
        (log-debug "sending packet ~a of ~a bytes (+ 4 1 ~a ~a ~a)" id total payload-length padding-length mac-length)
        (define sent : Natural (write-bytes-avail/enable-break packet sshout 0 total))
        (log-debug "~a: sent ~a bytes, ~a% done" id sent (~r (* 100 (/ sent total)) #:precision '(= 2)))))
  
    (define/private (exchange-identification [enable-break? : Boolean]) : Void ; <http://tools.ietf.org/html/rfc4253#section-4.2>
      (define-values (sshin sshout) ((if enable-break? tcp-connect/enable-break tcp-connect) hostname portno))
      (define rfc-banner : String (~a banner #:max-width 253))
        
      (with-handlers ([exn:fail? (rethrow exn:ssh 'ssh-handshake "failed sending identification")])
        ; NOTE: RFC does not define the order who initaites the exchange process,
        ;       nonetheless, the client sends first is always not bad.
        (fprintf sshout "~a~a~a" rfc-banner #\return #\linefeed)
        (log-debug "sending identification: ~a" rfc-banner))
      
      (unless (sync/timeout/enable-break 1.618 sshin)
        (throw exn:ssh 'ssh-handshake "failed getting identification: timed out"))
        
      (let/cc break : Any
        (for ([line (in-port (lambda [[in : Input-Port]] (read-line in 'linefeed)) sshin)])
          (cond [(false? (and (string? line) (regexp-match? #px"^SSH-" line))) (log-debug (string-trim line))] ; TODO: filter terminal control chars
                [else (match-let ([(list-rest _ protoversion softwareversion comments) (string-split line #px"-|\\s")])
                        (unless (member protoversion (list "1.99" "2.0"))
                          ; NOTE: if server is older then client, then client should close connection
                          ;       and reconnect with the old protocol. It seems that the rules checking
                          ;       compatibility mode is not guaranteed.
                          (throw exn:ssh:unsupported 'ssh-handshake ["unknown SSH protocol: ~a" (string-trim line)]))
                        (log-debug "received identification: ~a" (string-trim line))
                        (break 'I-do-not-see-servers-that-send-data-after-this-process))])))
      
      (vector-set! /dev/sshio 0 sshin)
      (vector-set! /dev/sshio 1 sshout))
  
    (define/private (exchange-key) : Void ; <http://tools.ietf.org/html/rfc4253#section-7.1>
      (define crypt : Bytes (ssh-namelist->bytes ssh-algorithms/cipher))
      (define mac : Bytes (ssh-namelist->bytes ssh-algorithms/mac))
      (define compression : Bytes (ssh-namelist->bytes ssh-algorithms/compression))
      (define languages : Bytes (ssh-namelist->bytes null))

      (transport-send 'SSH_MSG_KEXINIT
                      (call-with-input-string (number->string (current-inexact-milliseconds)) md5-bytes) ; cookie
                      (ssh-namelist->bytes ssh-algorithms/kex)
                      (ssh-namelist->bytes ssh-algorithms/publickey)
                      crypt #| local |# crypt #| remote |#
                      mac #| local |# mac #| remote |#
                      compression #| local |# compression #| remote |#
                      languages #| local |# languages #| remote |#
                      (ssh-boolean->bytes #false) #| whether a guessed key exchange packet follows |#
                      (ssh-uint32->bytes 0) #| reserved, always 0 |#)
      )))

(module* main racket
  (require (submod ".."))

  (define show-debuginfo
    (lambda [session message attachment]
      (if (exn? attachment)
          (displayln attachment (current-error-port))
          (fprintf (current-output-port) "~a~n" message))))
  
  (for ([host (in-list (list "172.16.1.9" "172.16.1.6" "gyoudmon.org"))])
    (with-handlers ([exn? void])
      (define ssh (new ssh-session% [host host] [port 22] [on-debug show-debuginfo]))
      (send ssh collapse))))
