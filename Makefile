.PHONY: all compile shell

all: compile

compile:
	rebar3 compile

shell:
	rebar3 shell
