SHELL=bash

SOURCES := \
	grammar/AbsLang.hs \
	grammar/LexLang.hs \
	grammar/ParLang.hs \
	interpreter/Main.hs \
	interpreter/Interpreter.hs

all: $(SOURCES)
	cabal build

grammar/Makefile: grammar/lang.cf
	pushd grammar; \
	bnfc lang.cf --make; \
	popd;

grammar/AbsLang.hs: grammar/Makefile

grammar/LexLang.hs: grammar/Makefile
	pushd grammar; \
	alex -g LexLang.x; \
	popd;

grammar/ParLang.hs: grammar/Makefile
	pushd grammar; \
	happy -gca ParLang.y; \
	popd;

clean:
	cabal clean; \
	pushd grammar; \
	if [ -e Makefile ]; then \
		make distclean; \
	fi; \
	popd;

