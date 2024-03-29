.PHONY: compile xref eunit clean doc check make deps test rel

REBAR=./rebar

all: compile

# for busy typos
m: all
ma: all
mak: all
make: all

console: compile
	@erl -pa ebin
deps:
	@$(REBAR) update-deps get-deps

compile:
	@$(REBAR) compile

xref: compile
	@$(REBAR) xref

test: compile xref
	@./rebar skip_deps=true eunit

clean:
	@$(REBAR) clean

doc:
	@$(REBAR) doc

bench: compile
	@$(REBAR) eunit skip_deps=true suites=bench_tests

APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.secondsight_dialyzer_plt

check_plt: xref
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

build_plt: xref
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: xref
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin | \
	    fgrep -v -f ./dialyzer.ignore-warnings



check: compile xref
#	@echo "you need $(REBAR) build-plt before make check"
#	@$(REBAR) build-plt
	dialyzer --check
#	@$(REBAR) check-plt
#	@$(REBAR) dialyze

rel:
	cd rel && ../rebar generate

stage: rel
	rm -rf rel/secondsight/lib/secondsight* && ln -sf $(abspath apps/secondsight) rel/secondsight/lib

# @(foreach dep,$(wildcard deps/*), rm -rf rel/secondsight/lib/$(shell basename $(dep))* && ln -sf $(abspath $(dep)) rel/secondsight/lib;)
