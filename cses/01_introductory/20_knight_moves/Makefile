SHELL=bash

# About packages:
# https://downloads.haskell.org/ghc/latest/docs/users_guide/packages.html

# To see which packages are currently available, use the ghc-pkg list command

# To see which modules are provided by a package use
#   ghc-pkg field <package> exposed-modules

GHC := $(shell ghcup whereis ghc 9.12.4)

% : %.hs
	$(GHC) -o $@ $< -package containers -O -W -Wall -Wno-unused-top-binds -XSafe -XNoGeneralizedNewtypeDeriving

all : main

test : main test_input.txt test_output.txt
	@ TMP=$$(mktemp); \
	./main < test_input.txt | tee $$TMP ; \
	if cmp $$TMP <(sed 1d test_output.txt); \
	then echo -e "\033[1m\033[32mPASS\033[0m"; \
	else echo -e "\033[1m\033[31mFAIL\033[0m"; cat test_output.txt; \
	fi; \
	rm -f $$TMP

t : main test_input.txt test_output.txt
	diff --color=auto -y test_output.txt <(echo ACTUAL; timeout 3 ./main < test_input.txt)

clean :
	rm -f main *.o *.hi
