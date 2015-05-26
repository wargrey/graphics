#!/bin/sh

tmpdir=$(dirname `perl -mFile::Temp -le "print File::Temp->new();"`);
perl -ne 'print $_ if /^SHELL/ .. eof;' $0 | make -C ${tmpdir} -rRf -;
exit $?;

######################## Makefile #######################
SHELL := $(shell which bash) -c -x
.ALLPHONY := racket swipl

export OSTYPE := $(shell uname -s)
export OSNAME := $(shell perl -e 'print $$^O;')
export MACHTYPE := $(shell uname -m)

.SILENT:
.SECONDEXPANSION:
.PHONY: all $(.ALLPHONY)
all: $(.ALLPHONY)

unexport MAKEFLAGS

export CPUS := 4
export CC := gcc
# LDFLAGS CPPFLAGS are set for MacOSX
export LDFLAGS := -L/usr/local/opt/readline/lib -L/usr/local/opt/gmp/lib
export CPPFLAGS := -I/usr/cal/opt/readline/include -I/usr/local/opt/gmp/include

Prefix := /opt
VerRegExp := \([0-9]\+\.\)\+[0-9]\+

racket_dir := GYDMracket
racket_bin0 := $(shell which racket)
racket_bin := $(if $(findstring 0,$(words $(racket_bin0))),$(Prefix)/$(racket_dir)/bin/racket,$(racket_bin0))
racket_verion := $(shell curl --compressed http://download.racket-lang.org/all-versions.html | grep -m 1 -o 'racket-minimal-v$(VerRegExp)' | tr -d a-z-)
racket_src := racket-minimal-$(racket_verion)-src-builtpkgs.tgz
racket_pkgs_linux := libfontconfig libcairo2 libpango1.0 libjpeg62 freeglut3
racket_pkgs := math html make images pict3d
racket_config_darwin := --enable-macprefix
racket_config_linux :=

prolog_dir := GYDMprolog
prolog_bin0 := $(shell which swipl)
prolog_bin := $(if $(findstring 0,$(words $(prolog_bin0))),$(Prefix)/$(prolog_dir)/bin/swipl,$(prolog_bin0))
prolog_version := $(shell curl http://www.swi-prolog.org/download/devel | grep -m 1 -o 'download/devel/src/pl-$(VerRegExp)' | tr -d 'a-z-/')
prolog_src := pl-$(prolog_version).tar.gz
prolog_skipped_pkgs := R odbc xpce jpl PDT jasmine

$(racket_src):
	curl -O "http://mirror.racket-lang.org/installers/$(racket_verion)/$(racket_src)"

$(Prefix)/$(racket_dir)/bin/racket: $(racket_src)
	rm -fr racket-$(racket_verion) && tar xzvf $(racket_src)
	cd racket-$(racket_verion)/src && ./configure --prefix=$(Prefix)/$(racket_dir) --enable-origtree $(racket_config_$(OSNAME))
	cd racket-$(racket_verion)/src && $(MAKE) -j$(CPUS)
	cd racket-$(racket_verion)/src && sudo $(MAKE) install

racket: $(if $(findstring 0,$(words $(racket_bin0))),$(racket_bin))
	test -n "$(racket_pkgs_$(OSNAME))" && sudo apt-get install --yes $(racket_pkgs_$(OSNAME))
	sudo $(addprefix $(dir $(racket_bin)),raco) pkg install --skip-installed --scope installation --deps search-auto $(racket_pkgs)
	$(racket_bin) -l racket/base -e '(printf "We have Racket ~a.~n" (version))'

$(prolog_src):
	curl -O http://www.swi-prolog.org/download/devel/src/$(prolog_src)

$(Prefix)/$(prolog_dir)/bin/swipl: $(prolog_src)
	rm -fr pl-$(prolog_version) && tar xzvf $(prolog_src)
	cd pl-$(prolog_version) && ./prepare --yes --all
	cd pl-$(prolog_version) && ./configure --prefix=$(Prefix)/$(prolog_dir) --enable-shared --enable-usrprofile --with-world $(addprefix --without-,$(prolog_skipped_pkgs))
	cd pl-$(prolog_version) && $(MAKE) -j$(CPUS)
	cd pl-$(prolog_version) && sudo $(MAKE) install

swipl: $(if $(findstring 0,$(words $(prolog_bin0))),$(prolog_bin))
	echo "main :-" > display-version.pl
	echo " 	current_prolog_flag(version_data, swi(M, I, P, []))," >> display-version.pl
	echo " 	format('We have SWI-Prolog ~w.~w.~w.\n', [M, I, P])." >> display-version.pl
	$(prolog_bin) -g main -s display-version.pl -t halt

### vim:ft=make

