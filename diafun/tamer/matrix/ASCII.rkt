#lang typed/racket/base

(provide (all-defined-out))

(require geofun/vector)
(require diafun/matrix)

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define non-printable-chars : (HashTable Integer (List String String))
  #hasheq(
    (0  . ("NUL"  "空字符(Null)"))
    (1  . ("SOH"  "标题开始"))
    (2  . ("STX"  "正文开始"))
    (3  . ("ETX"  "正文结束"))
    (4  . ("EOT"  "传输结束"))
    (5  . ("ENQ"  "询问"))
    (6  . ("ACK"  "确认"))
    (7  . ("BEL"  "响铃(\\a)"))
    (8  . ("BS"   "退格(\\b)"))
    (9  . ("HT"   "水平制表符(\\t)"))
    (10 . ("LF"   "换行(\\n)"))
    (11 . ("VT"   "垂直制表符"))
    (12 . ("FF"   "换页(\\f)"))
    (13 . ("CR"   "回车(\\r)"))
    (14 . ("SO"   "移出"))
    (15 . ("SI"   "移入"))
    (16 . ("DLE"  "数据链路转义"))
    (17 . ("DC1"  "设备控制1"))
    (18 . ("DC2"  "设备控制2"))
    (19 . ("DC3"  "设备控制3"))
    (20 . ("DC4"  "设备控制4"))
    (21 . ("NAK"  "否定确认"))
    (22 . ("SYN"  "同步空闲"))
    (23 . ("ETB"  "传输块结束"))
    (24 . ("CAN"  "取消"))
    (25 . ("EM"   "介质结束"))
    (26 . ("SUB"  "替换"))
    (27 . ("ESC"  "退出(Escape)"))
    (28 . ("FS"   "文件分隔符"))
    (29 . ("GS"   "组分隔符"))
    (30 . ("RS"   "记录分隔符"))
    (31 . ("US"   "单元分隔符"))
    (32 . ("SP"   "空格(Space)"))
    (127 . ("DEL"  "删除"))))

(define ascii-style : (Mtx-Style-Make Mtx-Entry-Style)
  (lambda [code indices]
    (define ch (integer->char (caddr indices)))
    
    (make-mtx-entry-style
     #:fill-paint (cond [(char-lower-case? ch) 'MistyRose]
                        [(char-upper-case? ch) 'AntiqueWhite]
                        [(char-numeric? ch) 'Cornsilk]
                        [(char-punctuation? ch) 'Honeydew]
                        [(char-symbolic? ch) 'MintCream]
                        [else 'WhiteSmoke]))))

(define ascii-desc : (Mtx-Entry Byte)
  (lambda [code style indices]
    (define ch (integer->char code))
    (define desc : Geo
      (assert (if (hash-has-key? non-printable-chars code)
                  (dia-block-text-brief (format "[~a]" (cadr (hash-ref non-printable-chars code))) style)
                  (dia-block-text-brief (string ch) style))))
    (define self : Geo (geo-text code #:color 'DimGrey))
    (define dh (geo-height desc))

    (geo-rb-superimpose
     (geo-cc-superimpose (geo-rectangle (* dh 4.0) (* dh 2.0) #:stroke #false #:fill #false)
                         desc)
     self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ascii
  ((inst dia-array Byte) #:ncols 8 #:gap 2.0
                         #:λstyle ascii-style #:desc ascii-desc
                         (range 128) 96 48))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  ascii)
