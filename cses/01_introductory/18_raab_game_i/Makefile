SHELL=bash

% : %.hs
	ghc -O -Wall -Wextra $<

all : main

# a diff can't work for this exercise because correct output can vary

# test : main test_input.txt test_output.txt
# 	@ TMP=$$(mktemp); \
# 	./main < test_input.txt | tee $$TMP ; \
# 	if cmp $$TMP test_output.txt; \
# 	then echo "\033[1m\033[32mPASS\033[0m"; \
# 	else echo "\033[1m\033[31mFAIL\033[0m - Expected:"; cat test_output.txt; \
# 	fi; \
# 	rm -f $$TMP

t : main 1.in 1.out
	./main --check 1.out < 1.in

t1 : main 1.in 1.out
	vimdiff 1.out <(timeout 3 ./main < 1.in)

clean :
	rm -f main *.o *.hi
