#lang at-exp typed/racket

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

@require{syntax.rkt}

(require/typed racket/base
               [string-port? (-> Port Boolean)]
               [port-progress-evt (-> Input-Port (Option (-> (Evtof Input-Port))))])

(define-type SSH-Transport-Recv-Callback (Rec x (-> (HashTable Natural Output-Port) x)))

(define ssh-custodian : Custodian (make-custodian))

(struct exn:ssh exn:fail ())
(struct exn:ssh:eof exn:ssh ([reason : SSH-Disconnection-Reason]))
(struct exn:ssh:again exn:ssh ())

(define-type/enum ssh-protocols : SSH-Protocol 2.0)

;;; Message Constants <http://tools.ietf.org/html/rfc4250#section-4.1>
(define-type/consts sm : SSH-Message-Type of Byte as packet:ssh
  ; for http://tools.ietf.org/html/rfc4253
  [SSH_MSG_DISCONNECT                 1 ([reason : UInt32] [description : String] [language : String])]
  [SSH_MSG_IGNORE                     2 ([data : String])]
  [SSH_MSG_UNIMPLEMENTED              3 ([seq : UInt32])]
  [SSH_MSG_DEBUG                      4 ([display? : Boolean] [message : String] [language : String])]
  [SSH_MSG_SERVICE_REQUEST            5 ([name : Symbol])]
  [SSH_MSG_SERVICE_ACCEPT             6 ([name : Symbol])]
  [SSH_MSG_KEXINIT                   20 ([cookie : (nBytes 16)]
                                         [kex : (Listof SSH-Algorithm/Kex)]
                                         [publickey : (Listof SSH-Algorithm/Publickey)]
                                         [cipher/local : (Listof SSH-Algorithm/Cipher)]
                                         [cipher/remote : (Listof SSH-Algorithm/Cipher)]
                                         [mac/local : (Listof SSH-Algorithm/MAC)]
                                         [mac/remote : (Listof SSH-Algorithm/MAC)]
                                         [compression/local : (Listof SSH-Algorithm/Compression)]
                                         [compression/remote : (Listof SSH-Algorithm/Compression)]
                                         [language/local : (Listof Symbol)]
                                         [language/remote : (Listof Symbol)]
                                         [guessing-follow? : Boolean]
                                         [reserved/zero : UInt32])]
  [SSH_MSG_NEWKEYS                   21 ()]
  ; for http://tools.ietf.org/html/rfc4252
  [SSH_MSG_USERAUTH_REQUEST          50 ([username : Symbol] [service : Symbol] [method : Symbol] [extra : Bytes])]
  [SSH_MSG_USERAUTH_FAILURE          51 ([methods : (Listof Symbol)] [partially? : Boolean])]
  [SSH_MSG_USERAUTH_SUCCESS          52 ()]
  [SSH_MSG_USERAUTH_BANNER           53 ([message : String] [language : String])]
  ; for http://tools.ietf.org/html/rfc4254
  [SSH_MSG_GLOBAL_REQUEST            80 ([name : Symbol] [replay? : Boolean] [extra : Bytes])]
  [SSH_MSG_REQUEST_SUCCESS           81 ([extra : Bytes])]
  [SSH_MSG_REQUEST_FAILURE           82 ()]
  [SSH_MSG_CHANNEL_OPEN              90 ([type : Symbol] [partner : UInt32] [window-size : UInt32] [packet-upsize : UInt32] [extra : Bytes])]
  [SSH_MSG_CHANNEL_OPEN_CONFIRMATION 91 ([channel : UInt32] [partner : UInt32] [window-size : UInt32] [packet-upsize : UInt32] [extra : Bytes])]
  [SSH_MSG_CHANNEL_OPEN_FAILURE      92 ([channel : UInt32] [reason : UInt32] [descripion : String] [language : String])]
  [SSH_MSG_CHANNEL_WINDOW_ADJUST     93 ([channel : UInt32] [size : UInt32])]
  [SSH_MSG_CHANNEL_DATA              94 ([channel : UInt32] [data : String])]
  [SSH_MSG_CHANNEL_EXTENDED_DATA     95 ([channel : UInt32] [type : UInt32] [data : String])]
  [SSH_MSG_CHANNEL_EOF               96 ([channel : UInt32])]
  [SSH_MSG_CHANNEL_CLOSE             97 ([channel : UInt32])]
  [SSH_MSG_CHANNEL_REQUEST           98 ([channel : UInt32] [type : Symbol] [reply? : Boolean] [extra : Bytes])]
  [SSH_MSG_CHANNEL_SUCCESS           99 ([channel : UInt32])]
  [SSH_MSG_CHANNEL_FAILURE          100 ([channel : UInt32])])

(define-type/consts sd : SSH-Disconnection-Reason of UInt32 
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

(define-type/consts sc : SSH-Channel-Failure-Reason of UInt32 
  [SSH_OPEN_ADMINISTRATIVELY_PROHIBITED                1]
  [SSH_OPEN_CONNECT_FAILED                             2]
  [SSH_OPEN_UNKNOWN_CHANNEL_TYPE                       3]
  [SSH_OPEN_RESOURCE_SHORTAGE                          4])

(define-type/consts se : SSH-Channel-Data-Type of UInt32 
  [SSH_EXTENDED_DATA_STDERR                            1])

;;; SSH Datatype Representations <http://tools.ietf.org/html/rfc4251#section-5>
(define ssh-boolean->bytes : (-> Any Bytes)
  (lambda [bool]
    (if bool (bytes 1) (bytes 0))))

(define ssh-bytes->boolean : (-> Bytes [#:offset Natural] Boolean)
  (lambda [bbool #:offset [offset 0]]
    (false? (zero? (bytes-ref bbool offset)))))

(define ssh-uint32->bytes : (-> UInt32 Bytes)
  (lambda [u32]
    (integer->integer-bytes u32 4 #false #true)))

(define ssh-bytes->uint32 : (-> Bytes [#:offset Natural] UInt32)
  (lambda [bint #:offset [offset 0]]
    (cast (integer-bytes->integer bint #false #true offset (+ offset 4)) UInt32)))

(define ssh-uint64->bytes : (-> Nonnegative-Integer Bytes)
  (lambda [u64]
    (integer->integer-bytes u64 8 #false #true)))

(define ssh-bytes->uint64 : (-> Bytes [#:offset Natural] Nonnegative-Integer)
  (lambda [bint #:offset [offset 0]]
    (cast (integer-bytes->integer bint #false #true offset (+ offset 8)) UInt32)))

(define ssh-string->bytes : (-> String Bytes)
  (lambda [utf8]
    (bytes-append (ssh-uint32->bytes (string-utf-8-length utf8))
                  (string->bytes/utf-8 utf8))))

(define ssh-bytes->string : (-> Bytes [#:offset Natural] String)
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

(define ssh-bytes->mpint : (-> Bytes [#:offset Natural] Integer)
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

(define ssh-bytes->namelist : (-> Bytes [#:offset Natural] (Listof Symbol))
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

    (define-syntax (log-ssh stx)
      (syntax-case stx []
        [(_ level #:urgent urgent message ...)
         #'(log-message sshlog level #false (format/src message ...) urgent)]
        [(_ level message ...)
         #'(log-message sshlog level #false (format/src message ...) (current-continuation-marks))]))

    (define-syntax (throw/log stx)
        (syntax-case stx []
          [(_ argl ...)
           #'(with-handlers ([exn? (lambda [[e : exn]] (log-ssh 'debug #:urgent e (exn-message e)) (raise e))])
               (throw argl ...))]))

    (define ghostcat : Thread
      (parameterize ([current-custodian ssh-custodian])
        (thread (thunk (let dispatch ([/dev/tcpin : (Evtof (U Input-Port Nothing)) never-evt]
                                      [/dev/log (make-log-receiver sshlog 'debug)]
                                      [/dev/ssh/stdout ((inst make-immutable-hasheq Natural Output-Port))]
                                      [on-recv (letrec ([rec : SSH-Transport-Recv-Callback (lambda [o] rec)]) rec)])
                         (match (sync/enable-break /dev/tcpin /dev/log)
                           [(? input-port?) ; /dev/tcpin is only used to trigger the network event.
                            (with-handlers ([exn:ssh:eof? (lambda [e] (dispatch never-evt /dev/log /dev/ssh/stdout on-recv))])
                              (dispatch /dev/tcpin /dev/log /dev/ssh/stdout (on-recv /dev/ssh/stdout)))]
                           [(vector 'info message (cons (? exact-positive-integer? chid) (? output-port? chlout)) _)
                            (on-debug 'info message 'open-channel topic)
                            (when (hash-has-key? /dev/ssh/stdout chid) (close-output-port (hash-ref /dev/ssh/stdout chid)))
                            (dispatch /dev/tcpin /dev/log (hash-set /dev/ssh/stdout chid chlout) on-recv)]
                           [(vector 'info message (cons (? input-port? /dev/ssh/stdin) (? procedure? recv)) _)
                            (on-debug 'info message 'make-ssh-port topic)
                            (dispatch /dev/ssh/stdin /dev/log /dev/ssh/stdout (cast recv SSH-Transport-Recv-Callback))]
                           [(vector 'info message (and event (vector 'SSH_MSG_DISCONNECT _)) _)
                            (on-debug 'info message event topic)]
                           [(vector level message urgent _)
                            (on-debug level message urgent topic)
                            (dispatch /dev/tcpin /dev/log /dev/ssh/stdout on-recv)]))))))

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

    (with-handlers ([exn:ssh:eof? (lambda [[e : exn:ssh:eof]] (collapse (exn-message e) (exn:ssh:eof-reason e)) (raise e))]
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
          (transport-send (ssh:msg:disconnect ($#sd reason) description ""))
          (close-output-port /dev/sshout)))
      (log-ssh 'info #:urgent (vector 'SSH_MSG_DISCONNECT reason) description)
      (thread-wait ghostcat) ; make sure ssh always can read until it receives an RST or EOF.
      (unless (port-closed? /dev/sshin) (close-input-port /dev/sshin))
      (custodian-shutdown-all watchcat))
    
    (define/private (make-/dev/sshio [tcpin : Input-Port] [tcpout : Output-Port]) : (Values Input-Port Output-Port)
      ;;; WARNING: receiving and sending in this case are asymmetrical.

      ; http://tools.ietf.org/html/rfc4253#section-6
      #| uint32    packet_length  (the next 3 fields)               -
         byte      padding_length (in the range of [4, 255])         \ the size of these 4 fields should be multiple of
         byte[n1]  payload; n1 = packet_length - padding_length - 1  / 8 or cipher-blocksize whichever is larger
         byte[n2]  random padding; n2 = padding_length              -
         byte[m]   mac (Message Authentication Code - MAC); m = mac_length |#
      (define transport-send-packet : (-> Any Boolean Boolean True #| Details see the Racket Reference (make-output-port) |#)
        (lambda [raw always-nonblock-by-me-due-to-disabled-break break-always-disabled-by-racket]
          (define-values (id payload-raw) 
            (if (false? (packet:ssh? raw))
                (values 'SSH_MSG_IGNORE
                        (bytes-append (bytes ($#sm 'SSH_MSG_IGNORE))
                                      (string->bytes/utf-8 (~s raw))))
                (let* ([msgid : Byte ($#sm (cast (object-name raw) SSH-Message-Type))]
                       [SSH_MSG : SSH-Message-Type (cast ($%sm msgid) SSH-Message-Type)])
                  ;; Both the form of SSH_MSG_ and ssh:msg: are valid SSH-Message-Types,
                  ;; and the form of SSH_MSG_ is used by logging facility for readability.
                  (values SSH_MSG
                          (for/fold ([payload : Bytes (bytes msgid)])
                                    ([content (in-vector (vector-drop (struct->vector raw) 1))]
                                     [datatype (in-list ($:sm SSH_MSG))])
                            (bytes-append payload (match content
                                                    [(? bytes? bstr) bstr]
                                                    [(? boolean? b) (ssh-boolean->bytes b)]
                                                    [(? string? utf8) (ssh-string->bytes utf8)]
                                                    [(? symbol? ascii) (ssh-string->bytes (symbol->string ascii))]
                                                    [(? list?) (ssh-namelist->bytes (cast content (Listof Symbol)))]
                                                    [(? exact-integer? n) (match datatype
                                                                            ['Byte (bytes n)]
                                                                            ['UInt32 (ssh-uint32->bytes (cast n UInt32))]
                                                                            ['UInt64 (ssh-uint64->bytes (cast n UInt64))]
                                                                            [else (ssh-mpint->bytes n)])])))))))
        
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
              (values (cast (+ packet-draft random-length -4) UInt32) random-length)))

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
                  [else (log-ssh 'info #:urgent (vector id sent total)
                                 "sent ~a: ~a bytes, ~a% done" id sent (~r (* 100 (/ sent total)) #:precision '(= 2)))]))
          #true))

      (define packet-upsize : Positive-Integer 35000)
      (define packet-buffer : Bytes (make-bytes packet-upsize))
      (define-values (/dev/ssh/usrin /dev/ssh/usrout) (make-pipe-with-specials packet-upsize '/dev/ssh/usrin '/dev/ssh/usrout))

      (define (curry-recv [start : Natural]) : SSH-Transport-Recv-Callback
        (lambda [chlouts] (transport-recv-packet start chlouts)))
      
      (define-syntax (try-again/log stx)
        (syntax-case stx []
          [(_ start argl ...)
           #'(begin (log-ssh 'debug argl ...)
                    (curry-recv start))]))

      (define transport-recv-packet : (-> Natural (HashTable Natural Output-Port) SSH-Transport-Recv-Callback)
        (lambda [start /dev/channel/usrout] ; WARNING: This procedure will run in separate thread(fiber)
          (define received (with-handlers ([exn:fail:network? (lambda [[e : exn:fail:network]] e)])
                             (read-bytes-avail!* packet-buffer tcpin start packet-upsize)))
          (cond [(eof-object? received)
                 (close-output-port /dev/ssh/usrout)
                 (throw/log [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "connection closed by ~a" hostname)]
                [(exn:fail:network? received)
                 (close-output-port /dev/ssh/usrout)
                 (throw/log [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] (exn-message received))]
                [(procedure? received)
                 (try-again/log start "special value cannot be here!")]
                [else (let extract-next ([bufstart 0]
                                         [bufused (+ start received)])
                        (when (positive? bufstart)
                          (bytes-copy! packet-buffer 0 packet-buffer bufstart (+ bufstart bufused)))
                        ;;; These identifiers may bind to dirty values if received bytes are not enough,
                        ;;; nonetheless, they must be valid in their own condition.
                        (define total-length : Natural (+ (ssh-bytes->uint32 packet-buffer) message-authsize))
                        (define padding-length : Byte (bytes-ref packet-buffer 4))
                        (define payload-length : Integer (- total-length message-authsize padding-length 1))
                        (define max-position : Integer (+ payload-length 5))
                        (cond [(< bufused (max cipher-blocksize 4))
                               (try-again/log (max bufused 0) "more bytes is required to extract the packet length.")]
                              [(> total-length packet-upsize)
                               (throw/log [exn:ssh:eof 'SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT] 
                                          "bufferoverflow attack??? ~a bytes! packet is too large!" total-length)]
                              [(> total-length (- bufused 4))
                               (try-again/log bufused "more bytes is required.")]
                              [else (let* ([message-type : Byte (bytes-ref packet-buffer 5)]
                                           [id : (Option SSH-Message-Type) ($%sm message-type)])
                                      (with-handlers ([exn? void])
                                        (cond [(false? id) (throw/log exn:ssh "ignored packet: unknown type ~a" message-type)]
                                              [(let extract : (Listof Any) ([sdaolyap : (Listof Any) null] [pos : Natural 6] [types ($:sm id)])
                                                 (match types
                                                   [(? null?)
                                                    (cond [(= pos max-position) (reverse sdaolyap)]
                                                          [else (throw/log exn:ssh "ignored packet[~a]: inconsist length (~a/~a)" id pos max-position)])]
                                                   [(list 'Bytes) ; this packet has special datum.
                                                    (if (> pos max-position)
                                                        (throw/log exn:ssh "ignored packet[~a]: inconsist length (~a > ~a)" id pos max-position)
                                                        (reverse (cons (subbytes packet-buffer pos max-position) sdaolyap)))]
                                                   [(list 'Byte rest ...)
                                                    (extract (cons (bytes-ref packet-buffer pos) sdaolyap) (add1 pos) rest)]
                                                   [(list 'Boolean rest ...)
                                                    (extract (cons (ssh-bytes->boolean packet-buffer #:offset pos) sdaolyap) (add1 pos) rest)]
                                                   [(list (list 'nBytes (? exact-positive-integer? n)) rest ...)
                                                    (extract (cons (subbytes packet-buffer pos (+ pos n)) sdaolyap) (+ pos n) rest)]
                                                   [(list 'UInt32 rest ...)
                                                    (extract (cons (ssh-bytes->uint32 packet-buffer #:offset pos) sdaolyap) (+ pos 4) rest)]
                                                   [(list 'UInt64 rest ...)
                                                    (extract (cons (ssh-bytes->uint64 packet-buffer #:offset pos) sdaolyap) (+ pos 8) rest)]
                                                   [(list 'String rest ...)
                                                    (let ([str-length : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> str-length payload-length)
                                                        (throw/log exn:ssh "ignored packet[~a]: inconsist length string (~a > ~a)"
                                                                   id str-length payload-length))
                                                      (extract (cons (ssh-bytes->string packet-buffer #:offset pos) sdaolyap)
                                                               (+ pos str-length 4) rest))]
                                                   [(list 'Symbol rest ...)
                                                    (let ([sym-length : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> sym-length payload-length)
                                                        (throw/log exn:ssh "ignored packet[~a]: inconsist length symbol (~a > ~a)"
                                                                   id sym-length payload-length))
                                                      (extract (cons (string->symbol (ssh-bytes->string packet-buffer #:offset pos)) sdaolyap)
                                                               (+ pos sym-length 4) rest))]
                                                   [(list 'MPInteger rest ...)
                                                    (let ([mpint-length : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> mpint-length payload-length)
                                                        (throw/log exn:ssh "ignored packet[~a]: inconsist length mpint (~a > ~a)"
                                                                   id mpint-length payload-length))
                                                      (extract (cons (ssh-bytes->mpint packet-buffer #:offset pos) sdaolyap)
                                                               (+ pos mpint-length 4) rest))]
                                                   [(list (list 'Listof _) rest ...)
                                                    (let ([names-length : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> names-length payload-length)
                                                        (throw/log exn:ssh "ignored packet[~a]: inconsist length name list (~a > ~a)"
                                                                   id names-length payload-length))
                                                      (extract (cons (ssh-bytes->namelist packet-buffer #:offset pos) sdaolyap)
                                                               (+ pos names-length 4) rest))]))
                                               => (lambda [[payloads : (Listof Any)]]
                                                    (define packet : packet:ssh ($*sm id payloads))
                                                    (log-ssh 'info #:urgent (vector id (+ total-length 4))
                                                             "received packet ~a, ~a bytes in total (+ 4 1 ~a ~a ~a)."
                                                             id padding-length payload-length message-authsize)
                                                    (write-special packet /dev/ssh/usrout))]))
                                      (extract-next (+ bufstart 4 total-length) (- bufused total-length 4)))]))])))
    
      (log-ssh 'info #:urgent (cons tcpin (curry-recv 0)) "ssh ports are ready!")
      
      (values (make-input-port (string->symbol (format "ssh:~a" hostname))
                               /dev/ssh/usrin /dev/ssh/usrin
                               (thunk (tcp-abandon-port tcpin)))
              (make-output-port (string->symbol (format "ssh:~a" hostname))
                                (cast tcpout (Evtof Output-Port))
                                tcpout (thunk (tcp-abandon-port tcpout))
                                transport-send-packet)))

    (define/private (exchange-key) : Void
      ; <http://tools.ietf.org/html/rfc4253#section-7.1>
      (unless (input-port? (sync/timeout 0 /dev/sshin))
        (log-ssh 'debug "we send SSH_MSG_KEXINIT first"))

      (transport-send (ssh:msg:kexinit
                       (call-with-input-string (number->string (current-inexact-milliseconds)) md5-bytes) ; cookie
                       ssh-algorithms/kex
                       ssh-algorithms/publickey
                       ssh-algorithms/cipher #| local |# ssh-algorithms/cipher #| remote |#
                       ssh-algorithms/mac #| local |# ssh-algorithms/mac #| remote |#
                       ssh-algorithms/compression #| local |# ssh-algorithms/compression #| remote |#
                       null #| language local |# null #| language remote |#
                       #false #| whether a guessed key exchange packet follows |#
                       0 #| reserved, always 0 |#))
      
      (unless (input-port? (sync/timeout 3.14 /dev/sshin))
        (log-ssh 'debug "did not receive peek kexinit")))

    (define/private (transport-send [packet : Any]) : Void
      (void (write-special packet /dev/sshout)))))

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
