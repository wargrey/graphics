#lang scribble/lp2

@(require "tamer.rkt")

@(require (for-syntax "tamer.rkt"))

@handbook-story{The Era of Plaintext Transmission is Over!}

@margin-note{This is an additional part of @secref{infrastructure.rkt}}

This is an implementation of SSH-2 protocol in Pure Racket. 

@tamer-smart-summary[]

@chunk[|<digivice taming start>|
       (require "tamer.rkt")

       (tamer-taming-start)
       (define partner (tamer-partner->modpath "digitama/sshmon.rkt"))

       |<sshmon:*>|]

@handbook-scenario{The Transport Layer}

@handbook-appendix[]

@chunk[|<sshmon:*>|
       (module+ main (call-as-normal-termination tamer-prove))
       (module+ story
         (define-tamer-suite ssh2-transport "The Transport Layer"
           )

         (define-tamer-suite ssh2-authentication "The User Authentication Layer"
           )
         
         (define-tamer-suite ssh2-connection "The Connection Layer"
           ))]
