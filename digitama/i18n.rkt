#lang typed/racket

(provide speak current-tongue all-languages default-language)

(require "digicore.rkt")

;;; Although Racket has its own i18n solution, it is binded too close with DrRacket.
;;; So I write my own solution based on its idea, but more elegant.

;; The first regexp is used in Windows and the second is used on other platforms.
;; All regexps are compared to the result of (system-language+country).
(define-type Language-Table (Listof (List Symbol Regexp Regexp)))
(define default-table : Language-Table
    '((English             #rx"^en_"        #rx"^English_")
      (Spanish             #rx"^es_"        #rx"^Espanol_")
      (German              #rx"^de_"        #rx"^German_")
      (French              #rx"^fr_"        #rx"French_")
      (Dutch               #rx"nl_"         #rx"^Netherlands_")
      (Danish              #rx"^da_DK"      #rx"^Danish_")
      (Portuguese          #rx"^pt_"        #rx"Portuguese_")
      (Japanese            #rx"^ja_"        #rx"^Japan_")
      (Traditional-Chinese #rx"^zh_(HK|TW)" #rx"Chinese_(Hong|Taiwan)")
      (Simplified-Chinese  #rx"^zh_CN"      #rx"Chinese_China")
      (Russian             #rx"^ru_"        #rx"^Russian_")
      (Ukrainian           #rx"^uk_"        #rx"^Ukrainian_")
      (Korean              #rx"^ko_"        #rx"^Korean_")))

(define default-language : (-> Symbol)
  (lambda []
    (let ([system-lang (system-language+country)])
      (let check-next ([table : Language-Table default-table])
        (cond [(null? table) 'English]
              [else (match (car table)
                      [(list lang win unix)
                       (cond [(regexp-match win system-lang) lang]
                             [(regexp-match unix system-lang) lang]
                             [else (check-next (cdr table))])])])))))

(define current-tongue : (Parameterof Symbol) (make-parameter (default-language)))

(define all-languages : (-> (Listof Symbol))
  (lambda []
    (for/list : (Listof Symbol) ([tongue.rktl : Path (in-list (directory-list (digimon-tongue)))]
                                 #:when (equal? (filename-extension tongue.rktl) #"rktl"))
      (string->symbol (string-trim (path->string tongue.rktl) #px"\\.rktl")))))

(define speak : (-> Symbol [#:in Symbol] String)
  (let ([dicts : (HashTable Symbol (HashTable Symbol String)) (make-hasheq)])
    (lambda [word #:in [tongue (current-tongue)]]
      (unless (hash-has-key? dicts tongue)
        (define gnome-tongue.rktl (parameterize ([current-digimon (digimon-gnome)]) (build-path (digimon-tongue) (~a tongue #".rktl"))))
        (define this-tongue.rktl (build-path (digimon-tongue) (~a tongue #".rktl")))
        (define dictionary : (Listof (Pairof Symbol String))
          (append (with-handlers ([exn:fail:filesystem? (λ _ null)]
                                  [exn:fail:contract? (λ _ null)]
                                  [exn:fail:read? (λ _ (error 'tongue "~a is The God's Script!" gnome-tongue.rktl))]
                                  [exn:fail:contract? (λ _ (error 'tongue "~a is not a dictionary!" gnome-tongue.rktl))])
                    (cast (with-input-from-file gnome-tongue.rktl read) (Listof (Pairof Symbol String))))
                  (with-handlers ([exn:fail:filesystem? (λ _ null)]
                                  [exn:fail:contract? (λ _ null)]
                                  [exn:fail:read? (λ _ (error 'tongue "~a is The God's Script!" this-tongue.rktl))]
                                  [exn:fail:contract? (λ _ (error 'tongue "~a is not a dictionary!" this-tongue.rktl))])
                    (cast (with-input-from-file this-tongue.rktl read) (Listof (Pairof Symbol String))))))
        (hash-set! dicts tongue (make-immutable-hash dictionary)))
      (hash-ref (hash-ref dicts tongue) word
                (thunk (cond [(symbol=? tongue 'English) (string-replace (symbol->string word) "-" " ")]
                             [else (speak word #:in 'English)]))))))
