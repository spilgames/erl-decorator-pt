REBAR ?= rebar
ifneq ($(wildcard rebar),)
	REBAR := ./rebar
endif

.PHONY: clean test docs benchmark go quick dialyzer

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile
	$(REBAR) skip_deps=true xref

clean:
	$(REBAR) clean

test: compile
	$(REBAR) skip_deps=true eunit

go:
	erl -name decorator_pt -pa deps/*/ebin -pa ebin/ ${EXTRA_ARGS}

dialyzer:
	dialyzer -c ebin/ -Wunmatched_returns -Werror_handling -Wrace_conditions
