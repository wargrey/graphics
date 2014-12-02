#lang scribble/base

@title{Project Conventions}
How to build a @italic{Digital World}? Okay, we don@literal{'}t start with the @italic{File Island}, but we own some concepts from the @italic{Digimon Series}.

@section{Hierarchy}
@bold{Note} Project or Subprojects are organized as the @italic{digimons} within @bold{village}. 
Each project may be separated into several repositories within @bold{island}, @bold{tamer}, and so on. 

@(itemlist @item{@bold{The Toplevel Project itself} is a mystery that sets up all the prerequistes of the world.}
           @item{@bold{village} is the birth place of @italic{digimons}. Namely it works like @tt{src}.}
           @item{@bold{digitama} is the egg of @italic{digimons}. Namely it works like @tt{libraries} or @tt{frameworks}.}
           @item{@bold{island} is the living environment of @italic{digimons}. Namely it works like @tt{share} or @tt{collection}.}
           @item{>  >@italic{nature} defines @italic{digimons}@literal{'} look and feel.}
           @item{>  >@italic{stone} stores the ancient sources to be translated.}
           @item{@bold{d-ark} is the interface for users to talk with @italic{digimons}. Namely it works like @tt{bin}.}
           @item{@bold{tamer} is the interface for developers to train the @italic{digimons}. Namely it works like @tt{test}.})

@section{Version}
Obviousely, our @italic{digimons} have their own life cycle.

@(itemlist @item{[X] @bold{Baby I} is the first stage of @italic{digimons} evolution straight from her @italic{digitama}. Namely it@literal{'}s the @tt{Alpha Version}.})

@section{Make Targets}
These conventions are borrowed from the GUN Make.

@(itemlist @item{@bold{all}: Building the entire program without generating documentation. This should be the default target.}
           @item{@bold{install}: Building and installing the program with three categories: normal ones, pre-installation commands and post-installation commands.}
           @item{@bold{install-docs}: Generating and installing documentation.}
           @item{@bold{uninstall}: Delete all the installed files and documentation with three categories, just like the installation commands.}
           @item{@bold{mostlyclean}: Delete all files except that people normally don't want to reconstruct.}
           @item{@bold{clean}: Delete all files that are normally created by running make.}
           @item{@bold{distclean}: Delete all files that are not included in the distribution, even if the makefile itself cannot create these files.}
           @item{@bold{maintainer-clean}: Delete all files that can be reconstructed, including sources produced by Source Generator such as 'ctangle' and 'cweave'.}
           @item{@bold{docs}: Generating documentation. User must manually invoke it.}
           @item{@bold{dist}: Creating a distribution file of the source files.}
           @item{@bold{test}: Performing unit tests on the source file after building.}
           @item{@bold{check}: Performing self tests on the program this makefile builds after installing.}
           @item{@bold{installcheck}: Performing installation tests on the target system after installing.}
           @item{@bold{installdirs}: Creating the directories where files are installed, and their parent directories.})
