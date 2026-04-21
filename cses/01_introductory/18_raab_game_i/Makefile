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

t : main test_input.txt test_output.txt
	diff --color=always test_output.txt <(./main < test_input.txt)

t1 : main 1.in 1.out
	vimdiff <(head -30 1.out) <((echo 19; sed -n 2,20p 1.in) | timeout 3 ./main)

clean :
	rm -f main *.o *.hi
