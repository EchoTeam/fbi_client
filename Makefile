REBAR := $(shell which ./rebar || which rebar)

.PHONY: all clean test-unit test-ct check

all: get-deps compile

get-deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -fr .eunit

test-unit: all
	$(REBAR) eunit skip_deps=true
	
test-ct: all
	$(REBAR) ct skip_deps=true
	
check: test-unit test-ct
	
