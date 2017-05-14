SHELL=bash

SOURCES_GRAMMAR := \
	grammar/AbsLang.hs \
	grammar/LexLang.hs \
	grammar/ParLang.hs \
	grammar/PrintLang.hs \
	grammar/ErrM.hs \
	interpreter/Interpreter.hs \
	interpreter/Main.hs \
	interpreter/Value.hs \
	interpreter/State.hs

all: $(SOURCES_GRAMMAR)
	ghc --make $(SOURCES_GRAMMAR) -o main

cabal: $(SOURCES_GRAMMAR)
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
	popd; \
	pushd interpreter; \
	rm -rf *.o *.hi; \
	popd; \
	rm -f main
