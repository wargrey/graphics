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
VerRegExp := \([0-9]\+\.\)\+[0-9]\+

racket_dir := GYDMracket
racket_bin0 := $(shell which racket)
racket_bin := $(if $(findstring 0,$(words $(racket_bin0))),$(Prefix)/$(racket_dir)/bin/racket,$(racket_bin0))
racket_verion := $(shell curl --compressed http://download.racket-lang.org/all-versions.html | grep -m 1 -o 'racket-minimal-v$(VerRegExp)' | tr -d a-z-)
racket_src := racket-minimal-$(racket_verion)-src-builtpkgs.tgz
racket_pkgs_linux := libfontconfig libcairo2 libpango1.0 libjpeg62 freeglut3
racket_pkgs := math html make images pict3d
racket_config_solaris := --enable-places --enable-futures --disable-libffi
racket_config_darwin := --enable-macprefix
racket_config_linux :=

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

### vim:ft=make

