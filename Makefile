REBAR= `which ./rebar || rebar`

all:
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -fr .eunit

test:
	$(REBAR) eunit skip_deps=true
