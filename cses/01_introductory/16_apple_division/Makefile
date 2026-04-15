% : %.hs
	ghc -O -Wall -Wextra $<

all : main knapsack lattice mincoins

test : main test_input.txt
	@ TMP=$$(mktemp); \
	./main < test_input.txt | tee $$TMP ; \
	if cmp $$TMP test_output.txt; \
	then echo "\033[1m\033[32mPASS:\033[0m"; \
	else echo "\033[1m\033[31mFAIL\033[0m - Expected:"; cat test_output.txt; \
	fi; \
	rm -f $$TMP

clean :
	rm -f main knapsack lattice mincoins *.o *.hi
