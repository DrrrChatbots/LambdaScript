.PHONY: test

all:
	@spago build

run:
	@spago run

test:
	@spago test

repl:
	@spago repl

app:
	@spago bundle-app
