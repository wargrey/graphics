#lang scribble/base

@;{ ; mode line for makemd.rkt with format: (target dependent ...)
   (/readme.md)
}

@title{Nanomon}
Digging data from Chaos such as iPhone Backups

@section{Project Conventions}
@include-section{convention.scrbl}

@section{Subprojects}
@(itemlist @item{[@bold{Tencent}](village/tencent): Digging data from Tencent Caches}
           @item{[@bold{iPhone}](village/iphone): Digging data from iPhone Backups})
