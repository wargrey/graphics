#!/bin/sh

tmpdir=$(dirname `perl -mFile::Temp -le "print File::Temp->new();"`);
perl -ne 'print $_ if /^.SHELLFLAGS/ .. eof;' $0 | exec sudo make -C ${tmpdir} -rRf -;
exit $?;

######################## Makefile #######################
.SHELLFLAGS := -c -x
.ALLPHONY := racket swipl

export OSTYPE := $(shell uname -s)
export OSNAME := $(shell perl -e 'print $$^O;')
export MACHTYPE := $(shell uname -m)

.SILENT:
.SECONDEXPANSION:
.PHONY: all $(.ALLPHONY)

unexport MAKEFLAGS

export CPUS := 4
export CC := gcc
# LDFLAGS CPPFLAGS are set for MacOSX
export LDFLAGS := -L/usr/local/opt/readline/lib -L/usr/local/opt/gmp/lib
export CPPFLAGS := -I/usr/cal/opt/readline/include -I/usr/local/opt/gmp/include

Prefix := /opt

racket_dir := GYDMracket
racket_bin := $(shell test `which racket` && echo `which racket` || echo $(Prefix)/$(racket_dir)/bin/racket)
racket_verion := $(shell curl --compressed http://download.racket-lang.org/all-versions.html | grep -m 1 -o 'racket-minimal-v[0-9.]\+html' | tr -d 'a-z-' | sed 's/.$$//')
racket_src := racket-minimal-$(racket_verion)-src-builtpkgs.tgz
racket_pkgs := math html make images
racket_config_darwin := --enable-macprefix
racket_config_linux :=

prolog_dir := GYDMprolog
prolog_bin := $(shell test `which swipl` && echo `which swipl` || echo $(Prefix)/$(prolog_dir)/bin/swipl)
prolog_version := $(shell curl http://www.swi-prolog.org/download/devel | grep -m 1 -o 'src/pl-[0-9.]\+tar.gz' | tr -d 'a-z-/' | sed 's/..$$//')
prolog_src := pl-$(prolog_version).tar.gz
prolog_skipped_pkgs := R odbc xpce jpl PDT jasmine
prolog_version_pl := "display_version :-\n\tcurrent_prolog_flag(version_data, swi(M, I, P, [])),\n\tformat('~w.~w.~w', [M, I, P])."

all: $(.ALLPHONY)
	echo We have Racket `$(racket_bin) -l racket/base -e '(display (version))'`.
	echo We have SWI-Prolog `echo $(prolog_version_pl) > version.pl && $(prolog_bin) -g display_version -s version.pl -t halt`.

$(racket_src):
	curl -O "http://mirror.racket-lang.org/installers/$(racket_verion)/$(racket_src)"

$(Prefix)/$(racket_dir)/bin/racket: $(racket_src)
	rm -fr racket-$(racket_verion) && tar xzvf $(racket_src)
	cd racket-$(racket_verion)/src && ./configure --prefix=$(Prefix)/$(racket_dir) --enable-origtree $(racket_config_$(OSNAME))
	cd racket-$(racket_verion)/src && $(MAKE) -j$(CPUS) && $(MAKE) install

racket: | $(racket_bin)
	$(addprefix $(dir $(racket_bin)),raco) pkg install --skip-installed --scope installation --deps search-auto $(racket_pkgs)

$(prolog_src):
	curl -O http://www.swi-prolog.org/download/devel/src/$(prolog_src)

$(Prefix)/$(prolog_dir)/bin/swipl: $(prolog_src)
	rm -fr pl-$(prolog_version) && tar xzvf $(prolog_src)
	cd pl-$(prolog_version) && ./prepare --yes --all
	cd pl-$(prolog_version) && ./configure --prefix=$(Prefix)/$(prolog_dir) --enable-shared --enable-usrprofile --with-world $(addprefix --without-,$(prolog_skipped_pkgs))
	cd pl-$(prolog_version) && $(MAKE) && $(MAKE) install

swipl: | $(prolog_bin)

### vim:ft=make
