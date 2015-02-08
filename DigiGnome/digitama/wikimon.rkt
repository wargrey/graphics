#lang racket/base

(require net/http-client)

(require racket/list)
(require racket/draw)
(require racket/class)
(require racket/string)

(provide (all-defined-out))

(define kanas (hash 'ア 'a 'イ 'Test3 'ウ 'u 'エ 'e 'オ 'o 'ヤ 'ya 'ユ 'yu 'ヨ 'yo 'ワ 'wa 'ヲ 'wo 'ン 'Test4a 'ー 'chouon2 'ヴ 'vu
                    'カ 'ka3 'キ 'ki 'ク 'ku2 'ケ 'ke 'コ 'ko 'ガ 'ga 'ギ 'gi 'グ 'gu 'ゲ 'ge 'ゴ 'go
                    'サ 'sa 'シ 'shi 'ス 'su 'セ 'se 'ソ 'so 'ザ 'za 'ジ 'ji 'ズ 'zu 'ゼ 'ze 'ゾ 'zo
                    'タ 'ta2 'チ 'chi 'ツ 'tsu 'テ 'te 'ト 'to 'ダ 'da 'デ 'de 'ド 'do
                    'ナ 'na 'ニ 'ni 'ヌ 'nu 'ネ 'ne 'ノ 'no
                    'ハ 'ha 'ヒ 'hi3 'フ 'fu 'ヘ 'he 'ホ 'ho 'バ 'ba 'ビ 'bi 'ブ 'bu 'ベ 'be 'ボ 'bo 'パ 'pa 'ピ 'pi3 'プ 'pu 'ペ 'pe 'ポ 'po
                    'マ 'ma2 'ミ 'mi 'ム 'mu 'メ 'me 'モ 'mo
                    'ラ 'ra 'リ 'ri 'ル 'ru2 'レ 're 'ロ 'ro))
  
(define theirfaults '{ma2})
(define alphabets (hash #\q 'q2))
  
(define fields (hash 'NSp 'Naturespirits 'DS 'Deepsavers 'NSo 'Nightmaresoldiers 'WG 'Windguardians
                     'ME 'Metalempire 'VB 'Virusbusters 'DR 'Dragonsroar 'JT 'Jungletroopers))

(define wikimon-recv!
  {lambda [uri]
    (eprintf " [Input from http://wikimon.net~a]~n" uri)
    (define-values {status headers pipe} (http-sendrecv "wikimon.net" uri #:content-decode null))
    (unless (regexp-match? #px"\\s200\\sOK" status) (error (bytes->string/utf-8 status)))
    (list status headers pipe)})

(define wikimon-image
  {lambda [filename]
    (define image-uri (cond [(regexp-match? #px"^/" (format "~a" filename)) (format "~a" filename)]
                            [else (let ([rfile (format "/File:~a" filename)]
                                        [pxpng (pregexp (format "(?<=href..)/images[^>]+~a(?=.>)" filename))])
                                    (car (regexp-match* pxpng (third (wikimon-recv! rfile)))))]))
    (make-object bitmap% (third (wikimon-recv! image-uri)) 'unknown/alpha)})

(define wikimon-reference
  {lambda [diginame]
    (define namemon (string-titlecase (format "~a" diginame)))
    (define pxjp #px"[ぁ-ゖ゠-ヿ㐀-䶵一-鿋豈-頻（）]+")
    (define metainfo (map bytes->string/utf-8
                          (cdr (regexp-match (pregexp (string-append "⇨ English.+?<br\\s*/?>(.+?)\\s*</td>\\s*</tr>"
                                                                     ".+?<img alt=.([ァ-ー]+). src=.(/images/.+?jpg)"
                                                                     "(.+?)List of Digimon"
                                                                     ".+?Attack Techniques</span></h1>(.+?)<h1>"))
                                             (third (wikimon-recv! (string-append "/" namemon)))))))
    (define basicinfo (string-split (fourth metainfo) #px"レベル|型（タイプ）|属性|フィールド|グループ"))
    (define take-info {lambda [i] (if (< i (length basicinfo)) (list-ref basicinfo i) "")})
    (list namemon
          (second metainfo)
          (third metainfo)
          (regexp-match* pxjp (take-info 1))
          (regexp-match* pxjp (take-info 2))
          (regexp-match* #px"ワクチン|データ|ウィルス|フリー|バリアブル|ヴァリアブル|不明" (take-info 3))
          (regexp-match* #px"[Ａ-Ｚ]{2}[ａ-ｚ]?" (take-info 4))
          (regexp-match* #px"四聖獣|十二神将|ロイヤルナイツ|七大魔王|三大天使|オリンポス十二神|四大竜|十闘士|クラックチーム|D-ブリガード|「BAN-TYO」|三銃士|ビッグデスターズ" (take-info 5))
          (regexp-replace* #px"</?font.*?>" (first metainfo) "")
          (regexp-match* pxjp (fifth metainfo)))})