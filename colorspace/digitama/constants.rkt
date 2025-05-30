#lang typed/racket/base

(provide (all-defined-out))

(require math/matrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CIE-RGB-Weight-Factors (Matrix Flonum))
(define-type CIE-Ref.White (Immutable-Vector Flonum Flonum Flonum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define CIE-primary : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.49000 0.31000 0.20000
                                    0.17697 0.81240 0.01063
                                    0.00000 0.01000 0.99000)))

; For monitor, Daylight 6504K, bluish
(define CIE-sRGB-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.412453 0.357580 0.180423
                                    0.212671 0.715160 0.072169
                                    0.019334 0.119193 0.950227)))

(define CIE-XYZ->LMS-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.8190224379967030 0.3619062600528904 -0.1288737815209879
                                    0.0329836539323885 0.9292868615863434  0.0361446663506424
                                    0.0481771893596242 0.2642395317527308  0.6335478284694309)))

(define CIE-LMS->OKLAB-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.2104542683093140  0.7936177747023054 -0.0040720430116193
                                    1.9779985324311684 -2.4285922420485799  0.4505937096174110
                                    0.0259040424655478  0.7827717124575296 -0.8086757549230774)))

(define CIE-LMS->XYZ-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable  1.2268798758459243 -0.5578149944602171  0.2813910456659647
                                     -0.0405757452148008  1.1122868032803170 -0.0717110580655164
                                     -0.0763729366746601 -0.4214933324022432  1.5869240198367816)))

(define CIE-OKLAB->LMS-D65 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 1.0000000000000000  0.3963377773761749  0.2158037573099136
                                    1.0000000000000000 -0.1055613458156586 -0.0638541728258133
                                    1.0000000000000000 -0.0894841775298119 -1.2914855480194092)))


(define CIE-XYZn-D65 : CIE-Ref.White #(0.9504559270516716 1.0 1.0890577507598784))

; For printed paper, Daylight 5003K, yellowish
(define CIE-sRGB-D50 : CIE-RGB-Weight-Factors
  (vector->matrix 3 3
                  (vector-immutable 0.4361 0.3851 0.1431
                                    0.2225 0.7169 0.0606
                                    0.0139 0.0971 0.7141)))

(define CIE-XYZn-D50 : CIE-Ref.White #(0.9642956764295677 1.0 0.8251046025104602))
