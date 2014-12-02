#lang scribble/base

@title{Digital World}

@section{The Book Project}
This, the toplevel project itself, is the @bold{Extra Dimensional Space} built for developers to set up all the prerequistes of all subprojects.
@bold{Namely all subprojects can and should be built on their own.}

@section{Building Scripts}
@(itemlist @item{[@bold{prerequisites.sh}](prerequisites.sh): Build the latest Racket and Swi-Prolog from offical source.}
           @item{[@bold{makefile.rkt}](makefile.rkt): As its name shows, it is the alternative to Makefile.})

@section{Subprojects}
@(itemlist @item{[@bold{Sakuyamon}](https://github.com/digital-world/sakuyamon) is in charge of gyoudmon.org}
           @item{[@bold{Nanomon}](https://github.com/digital-world/nanomon) is a cyberpunk that digging data from Chaos})

@include-section{convention.scrbl}
