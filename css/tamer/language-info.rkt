#lang racket

(require pict)
(require pict/code)

(define css-info-okay? (read-language (open-input-string "#lang css")))

(codeblock-pict
 #<<EOF
#lang css

@charset "UTF-8";

:root {
   font: 12px/1.5; /* error property, font property requires font-family or system font */
   prelude: @text-icon("Prelude> " @make-font(#:face "Lucida Grande") #:color #3C803C #:height 100% #:trim\? false);
}

EOF
)
