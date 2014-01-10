REBAR= `which ./rebar || rebar`

.PHONY: all clean test-unit test-ct check

all:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -fr .eunit

test-unit:
	$(REBAR) eunit skip_deps=true
	
test-ct:
	$(REBAR) ct skip_deps=true
	
check: test-unit test-ct
	
