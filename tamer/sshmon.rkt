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
       (require "../digitama/sshmon.rkt")

       (tamer-taming-start)

       |<sshmon:*>|]

@handbook-scenario{The Transport Layer Protocol}

@deftech[#:key "SSH-TRANS"]{@hyperlink["https://tools.ietf.org/html/rfc4253"]{The Transport Layer Protocol}}
provides a confidential channel over an insecure network.
It performs server host authentication, key exchange, encryption, and integrity protection.
It also derives a unique session id that may be used by higher-level protocols
such as @itech{SSH-USERAUTH} and @itech{SSH-CONNECT}.

The primitive datatypes and their representations are defined in
@hyperlink["http://tools.ietf.org/html/rfc4251#section-5"]{RFC 4251}.

@tamer-note['ssh-datatype]
@chunk[|<testsuite: ssh datatype: byte and boolean>|
       (let-values ([(bt bf b1 b0) (values (bytes 1) (bytes 0) #"1" #"0")])
         (test-suite "octet"
                     (test-case "boolean is byte with value 1 or 0"
                                (check bytes=? (ssh-boolean->bytes #true) bt)
                                (check bytes=? (ssh-boolean->bytes #false) bf)
                                (check-true (ssh-bytes->boolean bt))
                                (check-false (ssh-bytes->boolean bf)))
                     (test-case "boolean should not be stored other than 1 or 0"
                                (check-true (ssh-bytes->boolean b1))
                                (check-true (ssh-bytes->boolean b0)))))]

@chunk[|<testsuite: ssh datatype: unsigned integer>|
       (let-values ([(u32 b32) (values #x29b7f4aa (bytes #x29 #xb7 #xf4 #xaa))]
                    [(u64 b64) (values #x9a378f9b2e332a7 (bytes #x09 #xa3 #x78 #xf9 #xb2 #xe3 #x32 #xa7))])
         (test-suite "unsigned integer"
                     (test-case "32bit unsigned integer"
                                (check bytes=? (ssh-uint32->bytes u32) b32)
                                (check = (ssh-bytes->uint32 b32) u32))
                     (test-case "64bit unsigned integer"
                                (check bytes=? (ssh-uint64->bytes u64) b64)
                                (check = (ssh-bytes->uint64 b64) u64))))]

@chunk[|<testsuite: ssh datatype: string and text>|
       (let-values ([(str0 bstr0) (values "" (bytes #x0 #x0 #x0 #x0))]
                    [(txt btxt) (values "λ" (bytes #x00 #x00 #x00 #x02 #xce #xbb))])
         (test-suite "unicode string"
                     (test-case "empty string"
                                (check bytes=? (ssh-string->bytes str0) bstr0)
                                (check string=? (ssh-bytes->string bstr0) str0))
                     (test-case "unicode text"
                                (check bytes=? (ssh-string->bytes txt) btxt)
                                (check string=? (ssh-bytes->string btxt) txt))))]

@chunk[|<testsuite: ssh datatype: multiple precision integer>|
       (let-values ([(0mpi b0mpi) (values #x0 (bytes #x00 #x00 #x00 #x00))]
                    [(1mpi b1mpi) (values #x80 (bytes #x00 #x00 #x00 #x02 #x00 #x80))]
                    [(+mpi +bmpi) (values #x9a378f9b2e332a7 (bytes #x00 #x00 #x00 #x08 #x09 #xa3 #x78 #xf9 #xb2 #xe3 #x32 #xa7))]
                    [(-mps -bmps) (values (- #x1234) (bytes #x00 #x00 #x00 #x02 #xed #xcc))]
                    [(-mpl -bmpl) (values (- #xdeadbeef) (bytes #x00 #x00 #x00 #x05 #xff #x21 #x52 #x41 #x11))])
         (test-suite "multiple precision integer"
                     (test-case "#b00..0"
                                (check bytes=? (ssh-mpint->bytes 0mpi) b0mpi)
                                (check = (ssh-bytes->mpint b0mpi) 0mpi))
                     (test-case "#b10..0"
                                (check bytes=? (ssh-mpint->bytes 1mpi) b1mpi)
                                (check = (ssh-bytes->mpint b1mpi) 1mpi))
                     (test-case "positive integer"
                                (check bytes=? (ssh-mpint->bytes +mpi) +bmpi)
                                (check = (ssh-bytes->mpint +bmpi) +mpi))
                     (test-case "negative integer"
                                (check bytes=? (ssh-mpint->bytes -mps) -bmps)
                                (check = (ssh-bytes->mpint -bmps) -mps)
                                (check bytes=? (ssh-mpint->bytes -mpl) -bmpl)
                                (check = (ssh-bytes->mpint -bmpl) -mpl))))]

@chunk[|<testsuite: ssh datatype: name list>|
       (let-values ([(str0 bstr0) (values '() (bytes #x0 #x0 #x0 #x0))]
                    [(str1 bstr1) (values '(zlib) (bytes #x00 #x00 #x00 #x04 #x7a #x6c #x69 #x62))]
                    [(strn bstrn) (values '(zlib none) (bytes #x00 #x00 #x00 #x09 #x7a #x6c #x69 #x62 #x2c #x6e #x6f #x6e #x65))])
         (test-suite "ascii name list"
                     (test-case "empty list"
                                (check bytes=? (ssh-namelist->bytes str0) bstr0)
                                (check equal? (ssh-bytes->namelist bstr0) str0))
                     (test-case "one element list is just an ascii string"
                                (check bytes=? (ssh-namelist->bytes str1) bstr1)
                                (check equal? (ssh-bytes->namelist bstr1) str1))
                     (test-case "comma-separated list"
                                (check bytes=? (ssh-namelist->bytes strn) bstrn)
                                (check equal? (ssh-bytes->namelist bstrn) strn))))]

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
           ))]
