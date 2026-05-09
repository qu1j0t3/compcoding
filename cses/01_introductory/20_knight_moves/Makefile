SHELL=bash

% : %.hs
	ghc -O -W -Wall -XSafe -XNoGeneralizedNewtypeDeriving $<

all : main

test : main test_input.txt test_output.txt
	@ TMP=$$(mktemp); \
	./main < test_input.txt | tee $$TMP ; \
	if cmp $$TMP test_output.txt; \
	then echo -e "\033[1m\033[32mPASS\033[0m"; \
	else echo -e "\033[1m\033[31mFAIL\033[0m - Expected:"; cat test_output.txt; \
	fi; \
	rm -f $$TMP

t : main test_input.txt test_output.txt
	diff --color=auto -y test_output.txt <(echo ACTUAL; timeout 3 ./main < test_input.txt)

clean :
	rm -f main *.o *.hi
