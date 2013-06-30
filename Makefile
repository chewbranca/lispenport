REBAR=$(PWD)/rebar

all: compile

compile: deps
	$(REBAR) compile

deps:
	$(REBAR) get-deps update-deps

run:
	erl -pa deps/*/ebin ebin -s lispenport
