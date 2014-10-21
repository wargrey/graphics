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

Prefix := /opt
VerRegExp := \([0-9]\+\.\)\+[0-9]\+

racket_dir := GYDMracket
racket_bin := $(shell racket_bin=`which racket`; test $$? && echo $${racket_bin} || echo $(Prefix)/$(racket_dir)/bin/racket)
racket_verion := $(shell curl --compressed http://download.racket-lang.org/all-versions.html | grep -m 1 -o 'racket-minimal-v$(VerRegExp)' | tr -d a-z-)
racket_src := racket-minimal-$(racket_verion)-src-builtpkgs.tgz
racket_pkgs := math html make images
racket_config_darwin := --enable-macprefix
racket_config_linux :=

$(racket_src):
	curl -O "http://mirror.racket-lang.org/installers/$(racket_verion)/$(racket_src)"

$(Prefix)/$(racket_dir)/bin/racket: $(racket_src)
	rm -fr racket-$(racket_verion) && tar xzvf $(racket_src)
	cd racket-$(racket_verion)/src && ./configure --prefix=$(Prefix)/$(racket_dir) --enable-origtree $(racket_config_$(OSNAME))
	cd racket-$(racket_verion)/src && $(MAKE) -j$(CPUS)
	cd racket-$(racket_verion)/src && sudo $(MAKE) install

racket: | $(racket_bin)
	sudo $(addprefix $(dir $(racket_bin)),raco) pkg install --skip-installed --scope installation --deps search-auto $(racket_pkgs)
	$(racket_bin) -l racket/base -e '(printf "We have Racket ~a.~n" (version))'

### vim:ft=make

