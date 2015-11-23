#lang at-exp typed/racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://tools.ietf.org/html/rfc4250, The Secure Shell Protocol Assigned Numbers              ;;;
;;; https://tools.ietf.org/html/rfc4251, The Secure Shell Protocol Architecture                  ;;;
;;; https://tools.ietf.org/html/rfc4252, The Secure Shell Authentication Protocol                ;;;
;;; https://tools.ietf.org/html/rfc4253, The Secure Shell Transport Layer Protocol               ;;;
;;; https://tools.ietf.org/html/rfc4254, The Secure Shell Connection Protocol                    ;;;
;;;                                                                                              ;;;
;;; https://tools.ietf.org/html/rfc4419, Diffie-Hellman Group Exchange                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))

(require typed/openssl/md5)

(require math/base)
(require math/number-theory)

@require{syntax.rkt}

(define ssh-custodian : Custodian (make-custodian))

(struct exn:ssh exn:fail ())
(struct exn:ssh:eof exn:ssh ([reason : SSH-Disconnection-Reason]))
(struct exn:ssh:again exn:ssh ([continuation : (U Natural (List Natural Natural Natural))]))

(define-type/enum ssh-protocols : SSH-Protocol 2.0)

;;; Message Constants http://tools.ietf.org/html/rfc4250#section-4.1
(define-type/consts sm : SSH-Message-Type of Byte as packet:ssh
  ;; for http://tools.ietf.org/html/rfc4253
  [SSH_MSG_DISCONNECT                 1 ([reason : UInt32] [description : String] [language : String])]
  [SSH_MSG_IGNORE                     2 ([data : String])]
  [SSH_MSG_UNIMPLEMENTED              3 ([seq : UInt32])]
  [SSH_MSG_DEBUG                      4 ([display? : Boolean] [message : String] [language : String])]
  [SSH_MSG_SERVICE_REQUEST            5 ([name : Symbol])]
  [SSH_MSG_SERVICE_ACCEPT             6 ([name : Symbol])]
  [SSH_MSG_KEXINIT                   20 ([cookie : (nBytes 16)]
                                         [kex : (Listof Symbol)] [publickey : (Listof Symbol)]
                                         [cipher->s : (Listof Symbol)] [cipher->c : (Listof Symbol)]
                                         [mac->s : (Listof Symbol)] [mac->c : (Listof Symbol)]
                                         [compression->s : (Listof Symbol)] [compression->c : (Listof Symbol)]
                                         [language->s : (Listof Symbol)] [language->c : (Listof Symbol)]
                                         [guessing-followed? : Boolean]
                                         [reserved/zero : UInt32])]
  [SSH_MSG_NEWKEYS                   21 ()]
  ;; http://tools.ietf.org/html/rfc4419#section-5
  ; TODO: The numbers 30-49 are key exchange specific and may be redefined by other kex methods
  [SSH_MSG_KEX_DH_GEX_REQUEST        34 ([minbits : UInt32] [nbits : UInt32] [maxbits : UInt32])]
  [SSH_MSG_KEX_DH_GEX_GROUP          31 ([large-safe-prime : MPInteger] [generator : MPInteger])]
  [SSH_MSG_KEX_DH_GEX_INIT           32 ([e : MPInteger])]
  [SSH_MSG_KEX_DH_GEX_REPLY          33 ([hostkey : (nBytes String)] [f : MPInteger] [s : (nBytes String)])]
  ;; for http://tools.ietf.org/html/rfc4252
  [SSH_MSG_USERAUTH_REQUEST          50 ([username : Symbol] [service : Symbol] [method : Symbol] [extra : Bytes])]
  [SSH_MSG_USERAUTH_FAILURE          51 ([methods : (Listof Symbol)] [partially? : Boolean])]
  [SSH_MSG_USERAUTH_SUCCESS          52 ()]
  [SSH_MSG_USERAUTH_BANNER           53 ([message : String] [language : String])]
  ;; for http://tools.ietf.org/html/rfc4254
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

;;; SSH Datatype Representations http://tools.ietf.org/html/rfc4251#section-5
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

; Raw memory area for those CStrings that cannot be converted to UTF-8 Strings
(define ssh-raw->bytes : (-> (nBytes String) Bytes)
  (lambda [raw]
    (bytes-append (ssh-uint32->bytes (bytes-length raw)) raw)))

(define ssh-bytes->raw : (-> Bytes [#:offset Natural] (nBytes String))
  (lambda [raw #:offset [offset 0]]
    (subbytes raw (+ offset 4) (+ offset 4 (ssh-bytes->uint32 raw #:offset offset)))))
;;; End SSH Datatype

;;; http://tools.ietf.org/html/rfc4250#section-4.11
;;; http://tools.ietf.org/html/rfc4251#section-9.3.8 [order ajusted]
;; http://tools.ietf.org/html/rfc4253#section-6.2
(define-type/enum ssh-algorithms/compression : SSH-Algorithm/Compression
  [none     REQUIRED        no compression]
  #;[zlib     OPTIONAL        ZLIB (LZ77) compression])

;; http://tools.ietf.org/html/rfc4253#section-6.3
(define-type/enum ssh-algorithms/cipher : SSH-Algorithm/Cipher
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

;; http://tools.ietf.org/html/rfc4253#section-6.4
(define-type/enum ssh-algorithms/mac : SSH-Algorithm/MAC
  [hmac-sha1-96 RECOMMENDED     first 96 bits of HMAC-SHA1 (digest length = 12, key length = 20)]
  [hmac-sha1    REQUIRED        HMAC-SHA1 (digest length = key length = 20)]
  [hmac-md5-96  OPTIONAL        first 96 bits of HMAC-MD5 (digest length = 12, key length = 16)]
  [hmac-md5     OPTIONAL        HMAC-MD5 (digest length = key length = 16)]
  [none         OPTIONAL        no MAC])

;; http://tools.ietf.org/html/rfc4253#section-6.6
(define-type/enum ssh-algorithms/publickey : SSH-Algorithm/Publickey
  [ssh-rsa           RECOMMENDED  sign   Raw RSA Key]
  [ssh-dss           REQUIRED     sign   Raw DSS Key]
  #;[pgp-sign-rsa      OPTIONAL     sign   OpenPGP certificates (RSA key)]
  #;[pgp-sign-dss      OPTIONAL     sign   OpenPGP certificates (DSS key)])

;; http://tools.ietf.org/html/rfc4419#section-3
(define-type/enum ssh-algorithms/kex : SSH-Algorithm/Kex
  [diffie-hellman-group-exchange-sha1 REQUIRED]
  #;[diffie-hellman-group-exchange-sha256 RECOMMENDED])

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

(define ssh-client% : SSH-Session<%>
  (class object% (super-new)
    (init host)
    (init [port 22]
          [protocol 2.0]
          [breakable? #true])

    (init-field [on-debug void])

    (define watchcat : Custodian (make-custodian ssh-custodian))

    (define topic : Symbol (string->symbol (format "#<~a:~a:~a>" (object-name this) host port)))
    (define sshlog : Logger (make-logger topic #false))
    (define ghostcat : Thread (parameterize ([current-custodian watchcat]) (thread (thunk (transport:sync-match-handle-loop)))))

    (define-syntax (ssh-log stx)
      (syntax-case stx []
        [(_ level #:urgent urgent message ...)
         #'(log-message sshlog level #false (format message ...) urgent)]
        [(_ level message ...)
         #'(log-message sshlog level #false (format message ...) (current-continuation-marks))]))

    (define hostname : String host)
    (define portno : Integer port)
    (define sshversion : SSH-Protocol protocol)
    (define identification : String (~a #:max-width 253 (format "SSH-~a-WarGrey_SSHmon_0.6 Racket-~a" sshversion (version))))
    (define/public session-name (lambda [] topic))
    
    (define cipher-blocksize : Byte 0)
    (define message-authsize : Byte 0)
    (define compression : SSH-Algorithm/Compression 'none)

    (define (~% [current : Real] [total : Real]) : String (~r (* 100 (/ current total)) #:precision '(= 2)))
    
    (define/public (collapse [description "disconnected by user"] [reason 'SSH_DISCONNECT_BY_APPLICATION])
      (with-handlers ([exn? void])
        (transport-send (SSH_MSG_DISCONNECT ($#sd reason) (string-replace description (string #\newline) (string #\space)) ""))
        (sync/enable-break (thread-dead-evt ghostcat))
        (thread-wait ghostcat))
      (custodian-shutdown-all watchcat))

    (define/private transport-send  : (-> Any [#:block? Any] Void)
      (lambda [raw #:block? [block? #false]]
        (thread-send ghostcat (cons raw (and block? #true))
                     (lambda [] (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST]
                                       "session has collapsed")))
        (unless (false? block?)
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
      (define send-buffer : Bytes (make-bytes packet-upsize))
      (define recv-buffer : Bytes (make-bytes packet-upsize))

      (define dh-gex : (Vector String String Bytes  Bytes  Bytes   UInt32 UInt32 UInt32 MPInteger MPInteger MPInteger MPInteger MPInteger)
        (vector                "V_C"  "V_S"  #"I_C" #"I_S" #"K_S"  1024   0 #|n|#8196   0 #|p|#   0 #|g|#   0 #|e|#   0 #|f|#   0 #|x or K|#))

      (define negotiate-algorithms : (-> (Listof Symbol) (Listof Symbol) Symbol (Listof Symbol))
        (lambda [candidates proposed hint]
          ;; naming convention: http://tools.ietf.org/html/rfc4251#section-6
          ; TODO: deal with 'name@domainname'
          (define agreed : (Listof Symbol)
            ((inst filter-map Symbol Symbol)
             (lambda [[alg : Symbol]] (and (member alg proposed) alg))
             candidates))
          (when (null? agreed)
            (throw [exn:ssh:eof 'SSH_DISCONNECT_KEY_EXCHANGE_FAILED] "no more ~a algorithm available" hint))
          agreed))
      
      ; http://tools.ietf.org/html/rfc4253#section-6
      #| uint32    packet_length  (the next 3 fields)               -
         byte      padding_length (in the range of [4, 255])         \ the size of these 4 fields should be multiple of
         byte[n1]  payload; n1 = packet_length - padding_length - 1  / 8 or cipher-blocksize whichever is larger
         byte[n2]  random padding; n2 = padding_length              -
         byte[m]   mac (Message Authentication Code - MAC); m = mac_length |#

      ; TODO: do not send specific packets during the process of negotiating algorithms
      (define transport-send-packet : (-> Output-Port packet:ssh (U 'cleared (List Natural Natural Natural)) Void)
        (lambda [tcpout packet continuation]
          (define SSH_MSG_ID : SSH-Message-Type (cast (object-name packet) SSH-Message-Type))
          (define-values (offset total-length payload-length padding-length)
            (match continuation
              [(list offset total padding)
               (values offset total (- total padding message-authsize 4 1) padding)]
              [_ (let ([payload : Bytes (for/fold ([payload : Bytes (bytes ($#sm SSH_MSG_ID))])
                                                  ([content (in-vector (vector-drop (struct->vector packet) 1))]
                                                   [datatype (in-list ($:sm SSH_MSG_ID))])
                                          (bytes-append payload (match content
                                                                  [(? boolean? b) (ssh-boolean->bytes b)]
                                                                  [(? string? utf8) (ssh-string->bytes utf8)]
                                                                  [(? symbol? ascii) (ssh-string->bytes (symbol->string ascii))]
                                                                  [(? list?) (ssh-namelist->bytes (cast content (Listof Symbol)))]
                                                                  [(? bytes? bstr) (match datatype
                                                                                          [(list 'nBytes 'Strng) (ssh-raw->bytes bstr)]
                                                                                          [else bstr])]
                                                                  [(? exact-integer? n) (match datatype
                                                                                          ['UInt32 (ssh-uint32->bytes (cast n UInt32))]
                                                                                          ['UInt64 (ssh-uint64->bytes (cast n UInt64))]
                                                                                          ['MPInteger (ssh-mpint->bytes n)]
                                                                                          [else (bytes n)])])))])
                   (when (> (bytes-length payload) 32768)
                     (throw exn:ssh "packet is too large to send."))
          
                   ;;; TODO: Compression

                   (when (SSH_MSG_KEXINIT? packet)
                     ; http://tools.ietf.org/html/rfc4419#section-3
                     (vector-set! dh-gex 2 payload))

                   (define payload-length : Natural (bytes-length payload))
                   (define-values (packet-length padding-length)
                     (let* ([idsize : Byte (max 8 cipher-blocksize)]
                            [packet-draft : Integer (+ 5 payload-length)]
                            [padding-draft : Integer (- idsize (remainder packet-draft idsize))]
                            [padding-draft : Integer (if (< padding-draft 4) (+ padding-draft idsize) padding-draft)]
                            [capacity : Integer (quotient (- #xFF padding-draft) (add1 idsize))] ; for thwarting traffic analysis
                            [random-length : Integer (+ padding-draft (if (< capacity 1) 0 (* idsize (random capacity))))])
                       (values (cast (+ packet-draft random-length -4) UInt32) random-length)))

                   (define mac : Bytes (make-bytes message-authsize))

                   (let ([pad0 : Integer (bytes-ref send-buffer padding-length)])
                     (bytes-set! send-buffer 4 padding-length)
                     (bytes-copy! send-buffer 0 (ssh-uint32->bytes packet-length) 0 4)
                     (bytes-copy! send-buffer 5 payload 0 payload-length)
                     (bytes-copy! send-buffer (+ 5 payload-length) recv-buffer pad0 (+ pad0 padding-length))
                     (bytes-copy! send-buffer (+ 5 payload-length padding-length) mac 0 message-authsize))
                  
                   (values 0 (+ 4 packet-length message-authsize) payload-length (max padding-length 0)))]))

          (define resent : (Option Natural) (write-bytes-avail* send-buffer tcpout offset total-length))
          (cond [(false? resent) (throw [exn:ssh:again (list 0 total-length padding-length)] "network is busy")]
                [(zero? resent) (throw [exn:ssh:again (list 0 total-length padding-length)] "network was busy")]
                [else (let ([sent (+ offset resent)])
                        (if (< sent total-length)
                            (throw [exn:ssh:again (list sent total-length padding-length)]
                                   "sending ~a, ~a bytes, ~a% done" SSH_MSG_ID sent (~% sent total-length))
                            (ssh-log 'info #:urgent (vector packet total-length)
                                     "sent ~a, ~a bytes in total (+ 4 1 ~a ~a ~a)"
                                     SSH_MSG_ID total-length payload-length padding-length message-authsize)))])))

      (define transport-recv-packet : (-> Input-Port Natural Nothing)
        (lambda [tcpin start]
          (define received (with-handlers ([exn:fail:network? (lambda [[e : exn:fail:network]] e)])
                             (read-bytes-avail! recv-buffer tcpin start packet-upsize)))
          (cond [(eof-object? received)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] "connection closed by ~a" hostname)]
                [(exn:fail:network? received)
                 (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST] (exn-message received))]
                [(procedure? received)
                 (throw [exn:ssh:again start] "special value should not be here!")]
                [else (let extract-packet ([bufstart : Natural 0]
                                           [bufused : Natural (+ start received)])
                        (when (positive? bufstart)
                          (bytes-copy! recv-buffer 0 recv-buffer bufstart (+ bufstart bufused)))
                        (when (zero? bufused)
                          (throw [exn:ssh:again 0] "no more packet to extract"))
                        (when (< bufused (max cipher-blocksize 4))
                          (throw [exn:ssh:again bufused] "receiving packet header, ~a out of ~a bytes" bufused (max cipher-blocksize 4)))
                        (define total-length : Natural (+ 4 (ssh-bytes->uint32 recv-buffer) message-authsize))
                        (define padding-length : Byte (bytes-ref recv-buffer 4))
                        (define payload-length : Integer (- total-length message-authsize padding-length 1 4))
                        (define payload-end : Integer (+ payload-length 5))
                        (define message-id : Byte (bytes-ref recv-buffer 5))
                        (define SSH_MSG_ID : (Option SSH-Message-Type) ($%sm message-id))
                        (when (> total-length packet-upsize)
                          (throw [exn:ssh:eof 'SSH_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT]
                                 "~a bytes! packet[~a] is corrupt in length!" total-length (or SSH_MSG_ID message-id)))
                        (when (> total-length bufused)
                          (throw [exn:ssh:again bufused]
                                 "receiving ~a, ~a bytes, ~a% done" (or SSH_MSG_ID message-id) bufused (~% bufused total-length)))
                        (with-handlers ([exn? (lambda [[e : exn]] (ssh-log 'debug #:urgent e "extracting ~a: ~a" (or SSH_MSG_ID message-id) (exn-message e)))])
                          (define payloads : (Listof Any)
                            (let extract : (Listof Any) ([sdaolyap : (Listof Any) null] [pos : Natural 6] [types (if SSH_MSG_ID ($:sm SSH_MSG_ID) null)])
                              (match types
                                [(? null?)
                                 (cond [(= pos payload-end) (reverse sdaolyap)]
                                       [else (throw exn:ssh "ignored ~a: corrupt size (~a/~a)" SSH_MSG_ID pos payload-end)])]
                                [(list 'Bytes) ; this packet has special datum.
                                 (cond [(<= pos payload-end) (reverse (cons (subbytes recv-buffer pos payload-end) sdaolyap))]
                                       [else (throw exn:ssh "ignored ~a: corrupt size (~a > ~a)" SSH_MSG_ID pos payload-end)])]
                                [(list 'Byte rest ...)
                                 (extract (cons (bytes-ref recv-buffer pos) sdaolyap) (add1 pos) rest)]
                                [(list 'Boolean rest ...)
                                 (extract (cons (ssh-bytes->boolean recv-buffer #:offset pos) sdaolyap) (add1 pos) rest)]
                                [(list (list 'nBytes (? exact-positive-integer? n)) rest ...)
                                 (extract (cons (subbytes recv-buffer pos (+ pos n)) sdaolyap) (+ pos n) rest)]
                                [(list 'UInt32 rest ...)
                                 (extract (cons (ssh-bytes->uint32 recv-buffer #:offset pos) sdaolyap) (+ pos 4) rest)]
                                [(list 'UInt64 rest ...)
                                 (extract (cons (ssh-bytes->uint64 recv-buffer #:offset pos) sdaolyap) (+ pos 8) rest)]
                                [(list 'String rest ...)
                                 (let ([strsize : UInt32 (ssh-bytes->uint32 recv-buffer #:offset pos)])
                                   (when (> strsize payload-length)
                                     (throw exn:ssh "ignored ~a: corrupt string size (~a > ~a)" SSH_MSG_ID strsize payload-length))
                                   (extract (cons (ssh-bytes->string recv-buffer #:offset pos) sdaolyap) (+ pos strsize 4) rest))]
                                [(list 'Symbol rest ...)
                                 (let ([symsize : UInt32 (ssh-bytes->uint32 recv-buffer #:offset pos)])
                                   (when (> symsize payload-length)
                                     (throw exn:ssh "ignored ~a: corrupt symbol size (~a > ~a)" SSH_MSG_ID symsize payload-length))
                                   (extract (cons (string->symbol (ssh-bytes->string recv-buffer #:offset pos)) sdaolyap) (+ pos symsize 4) rest))]
                                [(list 'MPInteger rest ...)
                                 (let ([mpsize : UInt32 (ssh-bytes->uint32 recv-buffer #:offset pos)])
                                   (when (> mpsize payload-length)
                                     (throw exn:ssh "ignored ~a: corrupt mpint size (~a > ~a)" SSH_MSG_ID mpsize payload-length))
                                   (extract (cons (ssh-bytes->mpint recv-buffer #:offset pos) sdaolyap) (+ pos mpsize 4) rest))]
                                [(list (list 'Listof _) rest ...)
                                 (let ([namesize : UInt32 (ssh-bytes->uint32 recv-buffer #:offset pos)])
                                   (when (> namesize payload-length)
                                     (throw exn:ssh "ignored ~a: corrupt namelist size (~a > ~a)" SSH_MSG_ID namesize payload-length))
                                   (extract (cons (ssh-bytes->namelist recv-buffer #:offset pos) sdaolyap) (+ pos namesize 4) rest))]
                                [(list (list 'nBytes 'String) rest ...)
                                 (let ([rawsize : UInt32 (ssh-bytes->uint32 recv-buffer #:offset pos)])
                                   (when (> rawsize payload-length)
                                     (throw exn:ssh "ignored ~a: corrupt raw memory area size (~a > ~a)" SSH_MSG_ID rawsize payload-length))
                                   (extract (cons (ssh-bytes->raw recv-buffer #:offset pos) sdaolyap) (+ pos rawsize 4) rest))])))
                          ; http://tools.ietf.org/html/rfc4253#section-11.4
                          (define packet : packet:ssh ($*sm (or SSH_MSG_ID message-id) payloads))
                          (ssh-log 'info #:urgent (vector total-length packet)
                                   "received ~a, ~a bytes in total (+ 4 1 ~a ~a ~a)"
                                   (or SSH_MSG_ID message-id) total-length payload-length padding-length message-authsize)
                          (when (SSH_MSG_KEXINIT? packet)
                            ; http://tools.ietf.org/html/rfc4419#section-3
                            (vector-set! dh-gex 3 (subbytes recv-buffer 5 payload-end))))
                        (extract-packet total-length (max (- bufused total-length) 0)))])))

      (define transport-handle-packet : (-> packet:ssh Any)
        (lambda [packet]
          (match packet
            [(SSH_MSG_KEXINIT _ peer-kex peer-publickey cipher/ctos _ mac/ctos _ compression/ctos _ lang/ctos _ guessing-followed? _)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed hostkey types: ~a" peer-publickey)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed key exchange algorithms: ~a" peer-kex)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed encryption algorithms: ~a" cipher/ctos)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed mac algorithms: ~a" mac/ctos)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed compression algorithms: ~a" compression/ctos)
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "peer proposed langtags: ~a" lang/ctos)
             (when guessing-followed? ; https://tools.ietf.org/html/rfc4253#section-7.1
               (ssh-log 'error #:urgent 'SSH_MSG_KEXINIT "peer has guessed key exchange packet followed, we have to handle it."))
             (match-define (list kex publickey cipher mac compression)
               (map negotiate-algorithms
                    (list ssh-algorithms/kex ssh-algorithms/publickey ssh-algorithms/cipher ssh-algorithms/mac ssh-algorithms/compression)
                    (list peer-kex peer-publickey cipher/ctos mac/ctos compression/ctos)
                    (list 'kex 'publickey 'encryption 'MAC 'compression)))
             (ssh-log 'debug #:urgent 'SSH_MSG_KEXINIT "we preferred algorithms: ~a"
                      ((inst map Symbol (Listof Symbol)) car (list kex publickey cipher mac compression)))
             (list (SSH_MSG_KEXINIT (call-with-input-string (number->string (current-inexact-milliseconds)) md5-bytes)
                                    kex publickey cipher cipher mac mac compression compression null null #false 0)
                   (case (car kex)
                     [(diffie-hellman-group-exchange-sha1) ; http://tools.ietf.org/html/rfc4419#section-3
                      (define minbits : UInt32 (vector-ref dh-gex 5))
                      (define maxbits : UInt32 (vector-ref dh-gex 7))
                      (define nbits : UInt32 (cast (random-integer minbits maxbits) UInt32))
                      (vector-set! dh-gex 6 nbits)
                      (SSH_MSG_KEX_DH_GEX_REQUEST minbits nbits maxbits)]))]
            [(SSH_MSG_KEX_DH_GEX_GROUP p g) ; http://tools.ietf.org/html/rfc4419#section-3
             (define x : MPInteger (random-integer 2 (arithmetic-shift (sub1 p) -1)))
             (define e : MPInteger (modular-expt g x p))
             (vector-set! dh-gex 8 p)
             (vector-set! dh-gex 9 g)
             (vector-set! dh-gex 10 e)
             (vector-set! dh-gex 12 x)
             ;; TODO: sometimes OpenSSH will not reply, complain this:
             ;          buffer_get_bignum2_ret: negative numbers not supported [preauth]
             ;          buffer_get_bignum2: buffer error [preauth]
             (list (SSH_MSG_KEX_DH_GEX_INIT e))]
            [(SSH_MSG_KEX_DH_GEX_REPLY K_S f s) ; http://tools.ietf.org/html/rfc4419#section-3
             ; TODO: client can also accept the key(K_S) without verification for practical reason.
             (define p : MPInteger (vector-ref dh-gex 8))
             (define x : MPInteger (vector-ref dh-gex 12))
             (define K : MPInteger (modular-expt f x p))
             (unless (<= 1 f (sub1 p))
               (throw [exn:ssh:eof 'SSH_DISCONNECT_KEY_EXCHANGE_FAILED]
                      "exchange value is not in range [1, p - 1]"))
             (vector-set! dh-gex 4 K_S)
             (vector-set! dh-gex 11 f)
             (vector-set! dh-gex 12 K)
             (void)]
            [(? SSH_MSG_NEWKEYS? key-exchange-done) (void (list key-exchange-done))]

            ; https://tools.ietf.org/html/rfc4253#section-11
            [(SSH_MSG_IGNORE thwart-traffic-analysis) (ssh-log 'debug #:urgent 'SSH_MSG_IGNORE thwart-traffic-analysis)]
            [(SSH_MSG_DEBUG show? message language) (ssh-log 'debug #:urgent (vector 'SSH_MSG_DEBUG show? language) message)]
            [(? SSH_MSG_DISCONNECT? peer-disconnected) peer-disconnected]
            [(?packet:ssh message-id) (list (SSH_MSG_UNIMPLEMENTED message-id))])))
      
      (define/private transport:sync-match-handle-loop : (-> Any)
        (lambda []
          (define /dev/log : Log-Receiver (make-log-receiver sshlog 'debug))
          (define /dev/pkt : (Evtof Any) (wrap-evt (thread-receive-evt) (lambda [e] (thread-receive))))
          (define channels : SSH-Channels (make-hasheq))

          (define (make-pullout-evt [tcpout : (Evtof (U Output-Port Nothing))]) : (Evtof Any)
            (replace-evt tcpout (lambda [o] /dev/pkt)))
          
          (let dispatch ([/dev/tcpin : (Evtof (U Input-Port Nothing)) never-evt]       ; /dev/tcpin and /dev/tcpout are always left as 
                         [$in : (U 'plain Natural EOF) 'plain]                         ;   their original status, so that they can be closed
                         [/dev/tcpout : Output-Port (open-output-nowhere)]             ;   by one piece of code.
                         [$out : (U 'cleared (List Natural Natural Natural) EOF) eof]  ; even if output port is blocking,
                         [packet-evt : (Evtof Any) /dev/pkt])                          ;   mailbox still should be checked to trigger the destructor
            (match (sync/enable-break /dev/log (if (eof-object? $in) never-evt /dev/tcpin) packet-evt)
              [(vector (? symbol? level) (? string? message) urgent _)
               (match (vector level message urgent)
                 [(vector 'info message (vector (? exact-positive-integer? traffic) (? packet:ssh? packet)))
                  (on-debug 'info message (cons 'received traffic) topic)
                  (match (with-handlers ([exn? values]) (transport-handle-packet packet))
                    [(list (? packet:ssh? kexbacks) ...)
                     (thread-rewind-receive (map (lambda [kexback] (cons kexback #false)) (reverse kexbacks)))
                     (dispatch /dev/tcpin 0 /dev/tcpout $out packet-evt)]
                    [(or (? exn? e) ; we disconnect and give SSH_MSG_DISCONNECT a chance to be sent if exn:ssh:eof?
                         (? SSH_MSG_DISCONNECT? e)) ; peer disconnect, no packet will be sent, but still deals with log events
                     (ssh-sendmail 'recvback e)
                     (dispatch /dev/tcpin eof /dev/tcpout (if (SSH_MSG_DISCONNECT? e) eof $out) /dev/pkt)]
                    [_ (dispatch /dev/tcpin 0 /dev/tcpout $out packet-evt)])]
                 [(vector 'info message (vector (? packet:ssh? packet) (? exact-positive-integer? traffic)))
                  (on-debug 'info message (cons 'sent traffic) topic)
                  (if (false? (SSH_MSG_DISCONNECT? packet))
                      (dispatch /dev/tcpin $in /dev/tcpout $out packet-evt)
                      (with-handlers ([exn? void])
                        #| Destructor |#
                        ; TODO: check if there are more log events
                        (on-debug 'info (SSH_MSG_DISCONNECT-description packet) 'SSH_MSG_DISCONNECT topic)
                        (when (tcp-port? /dev/tcpin) (tcp-abandon-port /dev/tcpin))
                        (when (tcp-port? /dev/tcpout) (tcp-abandon-port /dev/tcpout))))]
                 [(vector 'info message (vector (? string? our-identification) (? input-port? tcpin) (? output-port? tcpout)))
                  (on-debug 'info message our-identification topic)
                  (vector-set! dh-gex 0 our-identification)
                  (match (with-handlers ([exn? values])
                           (fprintf tcpout "~a~a~a" our-identification #\return #\linefeed)
                           (flush-output tcpout))
                    [(exn message ccmark)
                     (ssh-sendmail 'sendback
                                   (exn:ssh:eof (string-append "failed to send identification: " message)
                                                ccmark 'SSH_DISCONNECT_CONNECTION_LOST))
                     (dispatch tcpin eof tcpout eof /dev/pkt)]
                    [else (dispatch tcpin 'plain tcpout 'cleared packet-evt)])]
                 [as-always
                  (on-debug level message urgent topic)
                  (dispatch /dev/tcpin $in /dev/tcpout $out packet-evt)])]
              [(? input-port? tcpin)
               (match (with-handlers ([exn? values])
                        (cond [(exact-nonnegative-integer? $in)
                               (transport-recv-packet tcpin $in)]
                              [(read-line tcpin 'linefeed)
                               => (lambda [[line/dont-trim : (U String EOF)]] : String
                                    (cond [(string? line/dont-trim) line/dont-trim]
                                          [else (throw [exn:ssh:eof 'SSH_DISCONNECT_CONNECTION_LOST]
                                                       "did not receive identification string from ~a" hostname)]))]))
                 [(and (exn:ssh:again _ _ (? exact-nonnegative-integer? received)) eagain)
                  (unless (zero? received) (ssh-log 'debug #:urgent eagain (exn-message eagain)))
                  (dispatch /dev/tcpin (exn:ssh:again-continuation eagain) /dev/tcpout $out packet-evt)]
                 [(? exn? e) ; (read-line) would cause other exceptions besides exn:ssh:eof.
                  (ssh-log 'debug #:urgent e (exn-message e))
                  (ssh-sendmail 'recvback e)
                  (dispatch /dev/tcpin eof /dev/tcpout $out /dev/pkt)]
                 [(and (? string?) (pregexp #px"^SSH-([^-]+)-(\\S+)(.*)$" (list identification\r protocol software comment\r)))
                  ; http://tools.ietf.org/html/rfc4253#section-4.2
                  (vector-set! dh-gex 1 (string-trim identification\r))
                  (ssh-sendmail 'recvback (list (vector-ref dh-gex 1) protocol software (string-trim (~a comment\r))))
                  (sync/timeout 0 never-evt) ; make sure client handles identification before receiving SSH_MSG_KEXINIT in localhost
                  (dispatch /dev/tcpin 0 /dev/tcpout $out /dev/pkt)]
                 [(? string? line)
                  ; TODO: RFC says control chars should be filtered
                  (on-debug 'debug (string-trim line) (void) topic)
                  (dispatch /dev/tcpin 'plain /dev/tcpout $out /dev/pkt)])]
              [(cons (? packet:ssh? packet) (? boolean? block?))
               (match (unless (eof-object? $out)
                        (with-handlers ([exn? values])
                          (transport-send-packet /dev/tcpout packet $out)))
                 [(and (exn:ssh:again _ _ (and (list sent total padded) continuation)) eagain)
                  (ssh-log 'debug #:urgent eagain (exn-message eagain))
                  (thread-rewind-receive (list (cons packet block?)))
                  (dispatch /dev/tcpin (if (SSH_MSG_DISCONNECT? packet) eof $in) ; Manually make TCP no longer reading
                            /dev/tcpout continuation (make-pullout-evt /dev/tcpout))]
                 [status ; TODO: handle network errors
                  (when (exn? status) (ssh-log 'debug #:urgent status "sending ~a: ~a" (object-name packet) (exn-message status)))
                  (when block? (ssh-sendmail 'sendback status))
                  (if (SSH_MSG_DISCONNECT? packet)
                      ; no more packets should be sent or received, but still deals with log events
                      (dispatch /dev/tcpin eof /dev/tcpout eof never-evt)
                      (dispatch /dev/tcpin $in /dev/tcpout 'cleared /dev/pkt))])]
              [unknown
               (thread-rewind-receive (list (cons (SSH_MSG_IGNORE (~s unknown)) #false)))
               (dispatch /dev/tcpin $in /dev/tcpout $out packet-evt)])))))

    ;;; Constructing
    (with-handlers ([exn:ssh:eof? (lambda [[e : exn:ssh:eof]] (collapse (exn-message e) (exn:ssh:eof-reason e)) (raise e))]
                    [exn:break? (lambda [[e : exn]] (collapse (exn-message e) 'SSH_DISCONNECT_AUTH_CANCELLED_BY_USER) (raise e))]
                    [exn? (lambda [[e : exn]] (collapse (exn-message e) 'SSH_DISCONNECT_RESERVED) (raise e))])
      (parameterize ([current-custodian watchcat])
        ;; initializing and handshaking
        (define-values (tcpin tcpout) ((if breakable? tcp-connect/enable-break tcp-connect) hostname portno))

        ; http://tools.ietf.org/html/rfc4253#section-4.2
        ; NOTE: RFC does not define the order who initaites the exchange process,
        ;       nonetheless, the client sends first is always not bad.
        (define message : String (string-append "sent identification: " identification))
        (ssh-log 'info #:urgent (vector identification tcpin tcpout) message)

        (let handshake : Void ()
          (match (sync/timeout/enable-break euler.0 /var/mail/send /var/mail/recv)
            [(? false?) (throw [exn:ssh:eof 'SSH_DISCONNECT_KEY_EXCHANGE_FAILED] "key exchange timeout")]
            [(vector _ _ (? exn? e) _) (raise e)]
            [(vector _ _ (list identification protoversion softwareversion comment) _)
             (unless (member protoversion (list "1.99" "2.0"))
               ; NOTE: if server is older then client, then client should close connection
               ;       and reconnect with the old protocol. It seems that the rules checking
               ;       compatibility mode is not guaranteed.
               (throw [exn:ssh:eof 'SSH_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED]
                      "unsupported protocol: ~a" identification))
             (ssh-log 'info "received identification: ~a" identification)
             (void (handshake))]
            [(vector _ _ (SSH_MSG_DISCONNECT reason description language) _)
             (throw [exn:ssh:eof 'SSH_DISCONNECT_BY_APPLICATION]
                    "received disconnect from ~a: ~a: ~a"
                    hostname reason description)]))))))

(module* main racket
  (require (submod ".."))

  (define show-debuginfo
    (lambda [level message extra session]
      (if (exn? extra)
          (fprintf (current-error-port) "[~a] ~a: ~a~n" (object-name extra) session message)
          (fprintf (current-output-port) "[~a] ~a: ~a~n" level session message))))
  
  (for ([host (in-list (list "localhost" "172.16.1.9"))]
        [port (in-list (list 8080 8080))])
    (with-handlers ([exn? void])
      (define ssh (new ssh-client% [host host] [port port] [on-debug show-debuginfo]))
      (send ssh collapse "demonstration done" 'SSH_DISCONNECT_BY_APPLICATION))))
