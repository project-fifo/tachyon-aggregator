REBAR = $(shell pwd)/rebar3
PROJECT=tachyon

.PHONY: rel package version all
all: version_header compile

include fifo.mk

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > $(PROJECT).version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat $(PROJECT).version)\">>)." > apps/$(PROJECT)/include/$(PROJECT)_version.hrl

clean:
	$(REBAR) clean
	make -C rel/pkg clean

long-test:
	[ -d apps/$(PROJECT)/.eunit ] && rm -r apps/$(PROJECT)/.eunit || true
	$(REBAR) skip_deps=true -DEQC_LONG_TESTS eunit -v -r

qc:
	$(REBAR) -C rebar_eqc.config compile skip_deps=true eunit --verbose

eqc-ci: clean all
	$(REBAR) -D EQC_CI -C rebar_eqc_ci.config compile eunit skip_deps=true --verbose

package: rel
	make -C rel/pkg package

rel:
	$(REBAR) as prod release
