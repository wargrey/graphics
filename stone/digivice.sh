#lang scribble/text

@(define digivice (vector-ref (current-command-line-arguments) 0))

#!/bin/sh

D=`dirname "$0"`
F=`basename "$0"`
cd "$D"
while test -L "$F"; do
  P=`readlink "$F"`
  D=`dirname "$P"`
  F=`basename "$P"`
  cd "$D"
done
D=`pwd`

exec "racket" "${F}-ark/${F}.rkt"  ${1+"$@|#\@|"}
