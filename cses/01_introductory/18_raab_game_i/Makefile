SHELL=bash

% : %.hs
	ghc -O -W -Wall -Wno-missing-signatures $<

all : main

# a simple diff can't work for this exercise because correct output can vary
# see runWithChecking in main.hs

# test : main test_input.txt test_output.txt
# 	@ TMP=$$(mktemp); \
# 	./main < test_input.txt | tee $$TMP ; \
# 	if cmp $$TMP test_output.txt; \
# 	then echo "\033[1m\033[32mPASS\033[0m"; \
# 	else echo "\033[1m\033[31mFAIL\033[0m - Expected:"; cat test_output.txt; \
# 	fi; \
# 	rm -f $$TMP

t1 : main 1.in 1.out
	./main --check 1.out < 1.in

t2 : main 2.in 2.out
	./main --check 2.out < 2.in

# t : main 1.in 1.out
# 	vimdiff 1.out <(timeout 3 ./main < 1.in)

clean :
	rm -f main *.o *.hi
