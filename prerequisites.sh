#!/bin/sh

tmpdir=$(dirname `perl -mFile::Temp -le "print File::Temp->new();"`);
perl -ne 'print $_ if /^SHELL/ .. eof;' $0 | make -C ${tmpdir} -rRf -;
exit $?;

######################## Makefile #######################
SHELL := $(shell which bash) -c -x
.ALLPHONY := racket 

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

racket_dir := PLTracket
racket_bin0 := $(shell which racket >/dev/null 2>/dev/null && which racket)
racket_bin := $(if $(findstring 0,$(words $(racket_bin0))),$(Prefix)/$(racket_dir)/bin/racket,$(racket_bin0))
racket_src := https://github.com/wargrey/racket/archive/master.zip
racket_pkgs_linux := libfontconfig libcairo2 libpango1.0 libjpeg62 freeglut3 libncurses5-dev
racket_pkgs := math html make images pict3d net-cookies
racket_config_solaris := --enable-places --enable-futures --disable-libffi
racket_config_darwin := --enable-macprefix
racket_config_linux :=

$(notdir $(racket_src)):
	wget $(racket_src)

$(Prefix)/$(racket_dir)/bin/racket: $(notdir $(racket_src))
	rm -fr racket-master; unzip $(notdir $(racket_src))
	cd racket-master/racket/src && ./configure --prefix=$(Prefix)/$(racket_dir) --enable-origtree $(racket_config_$(OSNAME))
	cd racket-master/racket/src && $(MAKE) -j$(CPUS)
	cd racket-master/racket/src && sudo $(MAKE) install

racket: $(if $(findstring 0,$(words $(racket_bin0))),$(racket_bin))
	test -n "$(racket_pkgs_$(OSNAME))" && sudo pkgin -y install $(racket_pkgs_$(OSNAME)) || true
	sudo $(dir $(racket_bin))/raco pkg install --skip-installed --scope installation --deps search-auto $(racket_pkgs)
	sudo $(dir $(racket_bin))/raco pkg install --jobs 1 --skip-installed --scope installation --deps search-auto whalesong
	$(racket_bin) -l racket/base -e '(printf "We have Racket ~a.~n" (version))'

### vim:ft=make
