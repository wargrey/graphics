#lang scribble/base

@;{ ; mode line for makemd.rkt with format: (target dependent ...)
   (readme.md)
}

@title{The Book}
Here is the @bold{Extra Dimensional Space} built for developers to set up all the prerequistes of the project itself.

@section{Project Conventions}
@include-section{convention.scrbl}

@section{The @bold{D-Ark} Scripts}
@(itemlist @item{[@bold{prerequisites.sh}](prerequisites.sh): Build the latest Racket from source.}
           @item{[@bold{makemd.rkt}](makemd.rkt): Make all the @tt{readme.md}s if updated.})
