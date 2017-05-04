SHELL=bash

all: main

grammar/Makefile: grammar/lang.cf
	pushd grammar; \
	bnfc lang.cf --make; \
	popd;

main: grammar/Makefile interpreter/Main.hs
	ghc --make -igrammar interpreter/Main.hs -o main

clean:
	rm -f \
		interpreter/*.hi \
		interpreter/*.o \
		main; \
	pushd grammar; \
	if [ -e Makefile ]; then \
		make distclean; \
		touch -m lang.cf; \
	fi; \
	popd;

