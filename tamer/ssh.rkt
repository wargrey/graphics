#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{The Era of Plaintext Transmission is Over!}

@margin-note{This is an additional part of @secref{infrastructure.rkt}}

This is an implementation of @deftech{@hyperlink["https://en.wikipedia.org/wiki/Secure_Shell#Version_2.x"]{SSH}}-2 protocol which has an
@hyperlink["https://tools.ietf.org/html/rfc4251"]{internal architecture} with well-separated layers:
@itemlist[#:style 'compact
          @item{@itech[#:key "SSH-TRANS"]{The Transport Layer Protocol} provides server authentication, confidentiality, and integrity with perfect forward secrecy.}
          @item{@itech[#:key "SSH-USERAUTH"]{The User Authentication Protocol} authenticates the client to the server.}
          @item{@itech[#:key "SSH-CONNECT"]{The Connection Protocol} multiplexes the encrypted tunnel into several logical channels.}]
   
@tamer-smart-summary[]

@chunk[|<sshmon taming start>|
       (require "tamer.rkt")
       (require "../digitama/ssh.rkt")

       (tamer-taming-start)

       |<sshmon:*>|]

@tamer-action[(hello-ssh/handshake "gyoudmon.org")]

@handbook-scenario{The Transport Layer Protocol}

@deftech[#:key "SSH-TRANS"]{@hyperlink["https://tools.ietf.org/html/rfc4253"]{The Transport Layer Protocol}}
provides a confidential channel over an insecure network.
It performs server host authentication, key exchange, encryption, and integrity protection.
It also derives a unique session id that may be used by higher-level protocols
such as @itech{SSH-USERAUTH} and @itech{SSH-CONNECT}.

@subsection{Datatype Representations}

The primitive datatypes are defined in @hyperlink["http://tools.ietf.org/html/rfc4251#section-5"]{RFC 4251}.

@tamer-note['ssh-datatype]
@chunk[|<testsuite: ssh datatype: byte and boolean>|
       (prove "octet" : boolean
              [#true  => [0x01] "#t is represented as byte 1"]
              [#false => [0x00] "#f is represented as byte 0"])]

@chunk[|<testsuite: ssh datatype: unsigned integer>|
       (prove "unsigned integer"
              [0x29b7f4aa        : uint32 => [0x29 0xb7 0xf4 0xaa] "32-bit unsigned integer"]
              [0x9a378f9b2e332a7 : uint64 => [0x09 0xa3 0x78 0xf9 0xb2 0xe3 0x32 0xa7] "64-bit unsigned integer"])]

@chunk[|<testsuite: ssh datatype: string and text>|
       (prove "unicode string" : string
              [""  => [0x00 0x00 0x00 0x00] "empty string"]
              ["λ" => [0x00 0x00 0x00 0x02 0xce 0xbb] "unicode text"])]

@chunk[|<testsuite: ssh datatype: multiple precision integer>|
       (prove "multiple precision integer" : mpint
              [0x0               => [0x00 0x00 0x00 0x00] "zero"]
              [0x9a378f9b2e332a7 => [0x00 0x00 0x00 0x08 0x09 0xa3 0x78 0xf9 0xb2 0xe3 0x32 0xa7] "positive integer"]
              [0x80              => [0x00 0x00 0x00 0x02 0x00 0x80] "positive integer [sign bit preceded]"]
              [-0x1234           => [0x00 0x00 0x00 0x02 0xed 0xcc] "negative integer"]
              [-0xdeadbeef       => [0x00 0x00 0x00 0x05 0xff 0x21 0x52 0x41 0x11] "negative integer [sign bit preceded]"])]

@chunk[|<testsuite: ssh datatype: name list>|
       (prove "ascii name list" : namelist
              ['()          => [0x00 0x00 0x00 0x00] "empty list"]
              ['(zlib)      => [0x00 0x00 0x00 0x04 0x7a 0x6c 0x69 0x62] "one element list is just an ascii string"]
              ['(zlib none) => [0x00 0x00 0x00 0x09 0x7a 0x6c 0x69 0x62 0x2c 0x6e 0x6f 0x6e 0x65] "comma-separated list"])]

@handbook-scenario{The User Authentication Protocol}

@deftech[#:key "SSH-USERAUTH"]{@hyperlink["https://tools.ietf.org/html/rfc4252"]{The User Authentication Protocol}}
provides a suite of mechanisms that can be used to authenticate the client user to the server.
Individual mechanisms specified in the authentication protocol use the session id provided by the @itech{SSH-TRANS}
and/or depend on the security and integrity guarantees of the @itech{SSH-TRANS}.

@handbook-scenario{The Connection Protocol}

@deftech[#:key "SSH-CONNECT"]{@hyperlink["https://tools.ietf.org/html/rfc4254"]{The Connection Protocol}}
defines the concept of channels, channel requests and global requests using which SSH services are provided.
A single SSH connection can host multiple channels simultaneously, each transferring data in both directions.
Channel requests are used to relay out-of-band channel-specific data, such as the changed size of a terminal window or the exit code of a server-side process.
The SSH client requests a server-side port to be forwarded using a global request. Standard channel types include:

@itemlist[#:style 'compact
          @item{shell for terminal shells, SFTP and exec requests (including SCP transfers)}
          @item{direct-tcpip for client-to-server forwarded connections}
          @item{forwarded-tcpip for server-to-client forwarded connections}]

@handbook-appendix{SSHmon Auxiliaries}

@chunk[|<sshmon:*>|
       (module+ main (call-as-normal-termination tamer-prove))
       (module+ story
         (require (for-syntax "tamer.rkt"))
         
         (define-syntax (prove stx)
           (syntax-case stx [=> :]
             [(_ suite-desc : datatype [rsrc => [0x ...] case-desc] ...)
              #'(prove suite-desc [rsrc : datatype => [0x ...] case-desc] ...)]
             [(_ suite-desc [rsrc : datatype => [0x ...] case-desc] ...)
              (with-syntax ([([datatype->bytes bytes->datatype dtval octets] ...)
                             (for/list ([dtsrc (in-list (syntax->list #'(rsrc ...)))]
                                        [subtype (in-list (syntax->list #'(datatype ...)))]
                                        [octetls (in-list (syntax->list #'([0x ...] ...)))])
                               (with-syntax ([dt->bs (string->symbol (format "ssh-~a->bytes" (syntax-e subtype)))]
                                             [bs->dt (string->symbol (format "ssh-bytes->~a" (syntax-e subtype)))]
                                             [dtval (if (identifier? dtsrc) (symb0x->number (syntax-e dtsrc)) dtsrc)]
                                             [octets (list->bytes (map (compose1 symb0x->number syntax-e) (syntax->list octetls)))])
                                 #'(dt->bs bs->dt dtval octets)))])
                #'(test-suite suite-desc
                              (test-case case-desc
                                         (check bytes=? (datatype->bytes dtval) octets)
                                         (check equal? (bytes->datatype octets) dtval))
                              ...))]))
         
         (define-tamer-suite ssh-datatype "The Primitive Datatype Representation"
           |<testsuite: ssh datatype: byte and boolean>|
           |<testsuite: ssh datatype: unsigned integer>|
           |<testsuite: ssh datatype: string and text>|
           |<testsuite: ssh datatype: multiple precision integer>|
           |<testsuite: ssh datatype: name list>|)
         
         (define-tamer-suite ssh-transport "The Transport Layer Protocol")

         (define-tamer-suite ssh-authentication "The User Authentication Protocol"
           )
         
         (define-tamer-suite ssh-connection "The Connection Protocol"
           )

         (define (hello-ssh/handshake host)
           (define (trace level message extra session)
             (if (exn? extra)
                 (fprintf (current-error-port) "[~a] ~a~n" level (exn-message extra))
                 (fprintf (current-output-port) "[~a] ~a~n" level message)))
           (define ssh-client (new ssh-session% [host "gyoudmon.org"] [port 22] [on-debug trace]))
           (send ssh-client collapse "demonstration done" 'SSH_DISCONNECT_BY_APPLICATION)))]
