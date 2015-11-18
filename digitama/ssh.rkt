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

(define ssh-custodian : Custodian (make-custodian))

(struct exn:ssh exn:fail ())
(struct exn:ssh:eof exn:ssh ([reason : SSH-Disconnection-Reason]))
(struct exn:ssh:again:send exn:ssh ([id : SSH-Message-Type] [stream : Bytes] [position : Natural]))
(struct exn:ssh:again:recv exn:ssh ([position : Natural]))

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
                                         [kex : (Listof Symbol)] [publickey : (Listof Symbol)]
                                         [cipher/local : (Listof Symbol)] [cipher/remote : (Listof Symbol)]
                                         [mac/local : (Listof Symbol)] [mac/remote : (Listof Symbol)]
                                         [compression/local : (Listof Symbol)] [compression/remote : (Listof Symbol)]
                                         [language/local : (Listof Symbol)] [language/remote : (Listof Symbol)]
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
               [breakable? Boolean #:optional])
         (init-field [on-debug (-> Symbol String Any Symbol Any) #:optional])
         [session-name (-> Symbol)]
         [collapse (->* () (String SSH-Disconnection-Reason) Void)]))

(define-type SSH-Channel<%>
  (Class (init [name Symbol])))

(define ssh-session% : SSH-Session<%>
  (class object% (super-new)
    (init host)
    (init [port 22]
          [protocol 2.0]
          [breakable? #true])

    (init-field [on-debug void])

    (define watchcat : Custodian (make-custodian ssh-custodian))

    (define topic : Symbol (string->symbol (format "#<~a:~a:~a>" (object-name this) host port)))
    (define sshlog : Logger (make-logger topic #false))
    (define ghostcat : Thread (parameterize ([current-custodian ssh-custodian]) (thread (thunk (transport:sync-match-dispatch-loop)))))

    (define-syntax (log-ssh stx)
      (syntax-case stx []
        [(_ level #:urgent urgent message ...)
         #'(if (exn? urgent)
               (log-message sshlog level #false (format/src #:stack (exn-continuation-marks urgent) message ...) urgent)
               (log-message sshlog level #false (format/src message ...) urgent))]
        [(_ level message ...)
         #'(log-message sshlog level #false (format/src message ...) (current-continuation-marks))]))

    (define-syntax (throw/log stx)
        (syntax-case stx []
          [(_ argl ...)
           #'(with-handlers ([exn? (lambda [[e : exn]] (log-ssh 'debug #:urgent e (exn-message e)) (raise e))])
               (throw argl ...))]))

    (define hostname : String host)
    (define portno : Integer port)
    (define sshversion : SSH-Protocol protocol)
    (define banner : String (format "SSH-~a-WarGrey_SSHmon_0.6 Racket-~a" sshversion (version)))
    (define/public session-name (lambda [] topic))
    
    (define cipher-blocksize : Byte 0)
    (define message-authsize : Byte 0)
    (define compression : SSH-Algorithm/Compression 'none)

    (define (~% [current : Real] [total : Real]) : String (~r (* 100 (/ current total)) #:precision '(= 2)))

    (with-handlers ([exn:ssh:eof? (lambda [[e : exn:ssh:eof]] (collapse (exn-message e) (exn:ssh:eof-reason e)) (raise e))]
                    [exn:break? (lambda [[e : exn]] (collapse (exn-message e) 'SSH_DISCONNECT_AUTH_CANCELLED_BY_USER) (raise e))]
                    [exn? (lambda [[e : exn]] (collapse (exn-message e) 'SSH_DISCONNECT_RESERVED) (raise e))])
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
          (log-ssh 'info "sent identification: ~a" rfc-banner))
        
        (let handshake : Void ()
          (define line : (U String EOF) (read-line tcpin 'linefeed))
          (cond [(eof-object? line)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "did not receive identification string from ~a" hostname)]
                [(false? (regexp-match? #px"^SSH-" line))
                 ; TODO: RFC says control chars should be filtered
                 (log-ssh 'debug (string-trim line))
                 (handshake)]
                [else (match-let ([(list-rest _ protoversion softwareversion comments) (string-split line #px"-|\\s")])
                        (unless (member protoversion (list "1.99" "2.0"))
                          ; NOTE: if server is older then client, then client should close connection
                          ;       and reconnect with the old protocol. It seems that the rules checking
                          ;       compatibility mode is not guaranteed.
                          (throw [exn:ssh:eof 'SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED] "unsupported protocol: ~a" (string-trim line)))
                        (log-ssh 'info #:urgent (vector 'SSH_MSG_KEXINIT tcpin tcpout)
                                 "received identification: ~a" (string-trim line)))]))))
    
    (define/public (collapse [description "job done"] [reason 'SSH_DISCONNECT_BY_APPLICATION])
      (with-handlers ([exn? void])
        (transport-send (ssh:msg:disconnect ($#sd reason) description "") #:block? #true))
      (log-ssh 'info #:urgent (vector 'SSH_MSG_DISCONNECT reason) description)
      (thread-wait ghostcat)
      (custodian-shutdown-all watchcat))

    (define/private (exchange-key) : Void
      ; <http://tools.ietf.org/html/rfc4253#section-7.1>
      (unless (vector? (sync/timeout 3.14 /var/mail/recv))
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
      
      (unless (vector? (sync/timeout 0 /var/mail/recv))
        (log-ssh 'debug "algorithm negotiation failed")))

    (define/private transport-send  : (-> Any [#:block? Any] Void)
      (lambda [raw #:block? [block? #false]]
        (thread-send ghostcat (cons raw (and block? #true)))
        (when block?
          (match (sync/enable-break /var/mail/send)
            [(vector 'error _ (? exn? e) _) (raise e)]))))

    (begin #| SSH Connection Protocol: this protocol is implemented with Racket logging facility |#
      #| It seems that the logging facility is a good choice to work as asynchronous channel:
           It has an internal topic-based dispatcher;
           It can deal with Racket values in a natural way;
           It's faster than pipe, special pipe, buffered asynchronous channel, ridiculous. |#
      (define-type SSH-Channels (HashTable Natural (Instance SSH-Channel<%>)))

      (define /dev/mail/recv : Logger (make-logger 'recvback #false))
      (define /dev/mail/send : Logger (make-logger 'sendback #false))
      (define /var/mail/recv : Log-Receiver (make-log-receiver /dev/mail/recv 'debug))
      (define /var/mail/send : Log-Receiver (make-log-receiver /dev/mail/send 'debug))

      (define-syntax (ssh-sendmail stx)
        (syntax-case stx [] ; meanwhile topic-based dispatcher is not available in Typed Racket
          [(_ topic v) #'(let ([/dev/mail (match topic ['recvback /dev/mail/recv] ['sendback /dev/mail/send])])
                           (cond [(exn? v) (log-message /dev/mail 'error topic (exn-message v) v)]
                                 [else (log-message /dev/mail 'info topic (~a v) v)]))])))

    (begin #| SSH Transport Layer Protocol: this protocol will run in separate thread(fiber) |#
      (define packet-upsize : Positive-Integer 35000)
      (define packet-buffer : Bytes (make-bytes packet-upsize))

      ; http://tools.ietf.org/html/rfc4253#section-6
      #| uint32    packet_length  (the next 3 fields)               -
         byte      padding_length (in the range of [4, 255])         \ the size of these 4 fields should be multiple of
         byte[n1]  payload; n1 = packet_length - padding_length - 1  / 8 or cipher-blocksize whichever is larger
         byte[n2]  random padding; n2 = padding_length              -
         byte[m]   mac (Message Authentication Code - MAC); m = mac_length |#
      
      (define transport-send-packet : (-> Output-Port packet:ssh Void)
        (lambda [tcpout packet]
          ;; Both the form of SSH_MSG_ and ssh:msg: are valid SSH-Message-Types,
          ;; and the form of SSH_MSG_ is used by logging facility for readability.
          (define msgid : Byte ($#sm (cast (object-name packet) SSH-Message-Type)))
          (define id : SSH-Message-Type (cast ($%sm msgid) SSH-Message-Type))
          (define payload-raw (for/fold ([payload : Bytes (bytes msgid)])
                                        ([content (in-vector (vector-drop (struct->vector packet) 1))]
                                         [datatype (in-list ($:sm id))])
                                (bytes-append payload (match content
                                                        [(? bytes? bstr) bstr]
                                                        [(? boolean? b) (ssh-boolean->bytes b)]
                                                        [(? string? utf8) (ssh-string->bytes utf8)]
                                                        [(? symbol? ascii) (ssh-string->bytes (symbol->string ascii))]
                                                        [(? list?) (ssh-namelist->bytes (cast content (Listof Symbol)))]
                                                        [(? exact-integer? n) (match datatype
                                                                                ['UInt32 (ssh-uint32->bytes (cast n UInt32))]
                                                                                ['UInt64 (ssh-uint64->bytes (cast n UInt64))]
                                                                                ['MPInteger (ssh-mpint->bytes n)]
                                                                                [else (bytes n)])]))))
        
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

          (with-handlers ([exn? (rethrow exn:ssh "failed to send ~a" id)])
            (define stream : Bytes (bytes-append (ssh-uint32->bytes packet-length) (bytes padding-length) payload random-padding mac))
            (define total : Natural (bytes-length stream))
            (log-ssh 'debug "sending ~a of ~a bytes (+ 4 1 ~a ~a ~a)" id total payload-length padding-length message-authsize)
            (define sent : (Option Natural) (write-bytes-avail* stream tcpout 0 total))
            (cond [(false? sent) (throw [exn:ssh:again:send id stream 0] "network is busy")]
                  [(zero? sent) (throw [exn:ssh:again:send id stream 0] "network was busy")]
                  [(< sent total) (throw [exn:ssh:again:send id stream sent] "sent ~a: ~a bytes, ~a% done" id sent (~% sent total))]
                  [else (log-ssh 'info #:urgent (vector id sent) "sent ~a: ~a bytes, 100% done" id sent)]))))

      (define transport-resend-packet : (-> Output-Port exn:ssh:again:send Void)
        (lambda [tcpout packet]
          (define id : SSH-Message-Type (exn:ssh:again:send-id packet))
          (define stream : Bytes (exn:ssh:again:send-stream packet))
          (define offset : Natural (exn:ssh:again:send-position packet))
          (define total : Natural (bytes-length stream))
          (with-handlers ([exn? (rethrow exn:ssh "failed to send ~a" id)])
            (define resent : (Option Natural) (write-bytes-avail* stream tcpout offset total))
            (cond [(false? resent) (throw [exn:ssh:again:send id stream offset] "network is still busy")]
                  [(zero? resent) (throw [exn:ssh:again:send id stream offset] "network was busy")]
                  [(zero? (- total resent offset)) (log-ssh 'info #:urgent (vector id total) "sent ~a: ~a bytes, 100% done" id total)]
                  [else (let ([sent (+ offset resent)])
                          (throw [exn:ssh:again:send id stream sent]
                                 "sent ~a: ~a bytes, ~a% done" id sent (~% sent total)))]))))

      (define transport-recv-packet : (-> Input-Port Natural SSH-Channels Void)
        (lambda [tcpin start /dev/channel/usrout]
          (define received (with-handlers ([exn:fail:network? (lambda [[e : exn:fail:network]] e)])
                             (read-bytes-avail! packet-buffer tcpin start packet-upsize)))
          (cond [(eof-object? received)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "connection closed by ~a" hostname)]
                [(exn:fail:network? received)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] (exn-message received))]
                [(procedure? received)
                 (throw [exn:ssh:again:recv start] "special value cannot be here!")]
                [else (let extract-packet ([bufstart : Natural 0]
                                           [bufused : Natural (+ start received)])
                        (when (positive? bufstart)
                          (bytes-copy! packet-buffer 0 packet-buffer bufstart (+ bufstart bufused)))
                        ;;; These identifiers may bind to dirty values if received bytes are not enough,
                        ;;; nonetheless, they must be valid in their own condition.
                        (define total-length : Natural (+ 4 (ssh-bytes->uint32 packet-buffer) message-authsize))
                        (define padding-length : Byte (bytes-ref packet-buffer 4))
                        (define payload-length : Integer (- total-length message-authsize padding-length 1 4))
                        (define max-position : Integer (+ payload-length 5))
                        (cond [(zero? bufused)
                               (throw [exn:ssh:again:recv 0] "no more packet to extract")]
                              [(< bufused (max cipher-blocksize 4))
                               (throw [exn:ssh:again:recv bufused] "need more bytes to extract packet length (~a < ~a)" bufused (max cipher-blocksize 4))]
                              [(> total-length packet-upsize)
                               (throw [exn:ssh:eof 'SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT]
                                      "bufferoverflow attack??? ~a bytes! packet is too large!" total-length)]
                              [(> total-length bufused)
                               (throw [exn:ssh:again:recv bufused] "receiving packet: ~a bytes, ~a% done" bufused (~% bufused total-length))]
                              [else (let* ([message-type : Byte (bytes-ref packet-buffer 5)]
                                           [id : (Option SSH-Message-Type) ($%sm message-type)])
                                      (with-handlers ([exn:ssh? void]
                                                      [exn? (lambda [[e : exn]] (log-ssh 'debug #:urgent e (exn-message e)))])
                                        (cond [(false? id) (throw exn:ssh "ignored packet: unknown type ~a, ~a bytes" message-type total-length)]
                                              [(let extract : (Listof Any) ([sdaolyap : (Listof Any) null] [pos : Natural 6] [types ($:sm id)])
                                                 (match types
                                                   [(? null?)
                                                    (cond [(= pos max-position) (reverse sdaolyap)]
                                                          [else (throw exn:ssh "ignored ~a: inconsistent size (~a/~a)" id pos max-position)])]
                                                   [(list 'Bytes) ; this packet has special datum.
                                                    (cond [(<= pos max-position) (reverse (cons (subbytes packet-buffer pos max-position) sdaolyap))]
                                                          [else (throw exn:ssh "ignored ~a: inconsistent size (~a > ~a)" id pos max-position)])]
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
                                                    (let ([strsize : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> strsize payload-length)
                                                        (throw exn:ssh "ignored ~a: bad string size (~a > ~a)" id strsize payload-length))
                                                      (extract (cons (ssh-bytes->string packet-buffer #:offset pos) sdaolyap) (+ pos strsize 4) rest))]
                                                   [(list 'Symbol rest ...)
                                                    (let ([symsize : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> symsize payload-length)
                                                        (throw exn:ssh "ignored ~a: bad symbol size (~a > ~a)" id symsize payload-length))
                                                      (extract (cons (string->symbol (ssh-bytes->string packet-buffer #:offset pos)) sdaolyap)
                                                               (+ pos symsize 4) rest))]
                                                   [(list 'MPInteger rest ...)
                                                    (let ([mpsize : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> mpsize payload-length)
                                                        (throw exn:ssh "ignored ~a: bad mpint size (~a > ~a)" id mpsize payload-length))
                                                      (extract (cons (ssh-bytes->mpint packet-buffer #:offset pos) sdaolyap) (+ pos mpsize 4) rest))]
                                                   [(list (list 'Listof _) rest ...)
                                                    (let ([namesize : UInt32 (ssh-bytes->uint32 packet-buffer #:offset pos)])
                                                      (when (> namesize payload-length)
                                                        (throw exn:ssh "ignored ~a: bad namelist size (~a > ~a)" id namesize payload-length))
                                                      (extract (cons (ssh-bytes->namelist packet-buffer #:offset pos) sdaolyap) (+ pos namesize 4) rest))]))
                                               => (lambda [[payloads : (Listof Any)]]
                                                    (define packet : packet:ssh ($*sm id payloads))
                                                    (log-ssh 'info #:urgent (vector id total-length)
                                                             "received ~a, ~a bytes in total (+ 4 1 ~a ~a ~a)"
                                                             id total-length padding-length payload-length message-authsize)
                                                    )]))
                                      (extract-packet total-length (max (- bufused total-length) 0)))]))])))

      (define/private transport:sync-match-dispatch-loop : (-> Any)
        (lambda []
          (define (make-sendevt [tcpout : (Evtof (U Output-Port Nothing))]) : (Evtof Any)
            (replace-evt tcpout (lambda [o] (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive))))))
          
          (define /dev/log : Log-Receiver (make-log-receiver sshlog 'debug))
          (define channels : SSH-Channels (make-hasheq))
          
          (let dispatch ([/dev/tcpin : (Evtof (U Input-Port Nothing)) never-evt]
                         [/dev/tcpout : Output-Port (open-output-nowhere)]
                         [sendevt : (Evtof Any) (make-sendevt never-evt)]
                         [bufstart : Natural 0])
            (match (sync/enable-break /dev/log /dev/tcpin sendevt)
              [(vector (? symbol? level) (? string? message) urgent _)
               (match (vector level message urgent)
                 ;[(vector 'info message (vector 'SSH_MSG_CHANNEL_OPEN (? exact-positive-integer? chid) (? output-port? chlout)))
                 ; (on-debug 'info message 'SSH_MSG_CHANNEL_OPEN topic)
                 ; (when (hash-has-key? /dev/ssh/stdout chid) (close-output-port (hash-ref /dev/ssh/stdout chid)))
                 ; (dispatch /dev/tcpin (hash-set /dev/ssh/stdout chid chlout))]
                 [(vector 'info message (vector 'SSH_MSG_KEXINIT (? input-port? tcpin) (? output-port? tcpout)))
                  (on-debug 'info message 'SSH_MSG_KEXINIT topic)
                  (dispatch tcpin tcpout (make-sendevt tcpout) bufstart)]
                 [(vector 'info message (vector 'SSH_MSG_DISCONNECT _))
                  (when (tcp-port? /dev/tcpin)  (tcp-abandon-port /dev/tcpin))
                  (when (tcp-port? /dev/tcpout) (tcp-abandon-port /dev/tcpout))
                  (on-debug 'info message 'SSH_MSG_DISCONNECT topic)]
                 [otherwise
                  (on-debug level message urgent topic)
                  (dispatch /dev/tcpin /dev/tcpout sendevt bufstart)])]
              [(? input-port? tcpin)
               (let ([status (with-handlers ([exn? values]) (transport-recv-packet tcpin bufstart channels))])
                 (when (exn? status) (log-ssh 'debug #:urgent status (exn-message status)))
                 (match status
                   [(? exn:ssh:eof?) (ssh-sendmail 'recvback status)]
                   [(exn:ssh:again:recv _ _ position) (dispatch /dev/tcpin /dev/tcpout sendevt position)]
                   [else (dispatch /dev/tcpin /dev/tcpout sendevt 0)]))]
              [(cons (and packet (or (? packet:ssh?) (? exn:ssh:again:send?))) (? boolean? block?))
               (let ([status (with-handlers ([exn? values])
                               (cond [(packet:ssh? packet) (transport-send-packet /dev/tcpout packet)]
                                     [else (transport-resend-packet /dev/tcpout packet)]))])
                 (when (exn? status) ; TODO: deal with network error
                   (log-ssh 'debug #:urgent status (exn-message status))
                   (when (exn:ssh:again:send? status)
                     (thread-rewind-receive (list (cons status block?)))))
                 (when (and block? (not (exn:ssh:again:send? status)))
                   (ssh-sendmail 'sendback status)))
               (dispatch /dev/tcpin /dev/tcpout sendevt bufstart)]
              [unknown
               (thread-rewind-receive (list (cons (ssh:msg:ignore (~s unknown)) #false)))
               (dispatch /dev/tcpin /dev/tcpout sendevt bufstart)])))))))

(module* main racket
  (require (submod ".."))

  (define show-debuginfo
    (lambda [level message extra session]
      (if (exn? extra)
          (fprintf (current-error-port) "[~a] ~a: ~a~n" (object-name extra) session (exn-message extra))
          (fprintf (current-output-port) "[~a] ~a: ~a~n" level session message))))
  
  (for ([host (in-list (list "localhost" "gyoudmon.org"))]
        [port (in-list (list 8080 22))])
    (with-handlers ([exn? void])
      (define ssh (new ssh-session% [host host] [port port] [on-debug show-debuginfo]))
      (send ssh collapse "demonstration done" 'SSH_DISCONNECT_BY_APPLICATION))))
