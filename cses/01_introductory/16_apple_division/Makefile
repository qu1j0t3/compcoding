main : main.hs
	ghc -Wall -Wextra main.hs

test : main test_input.txt
	@ TMP=$$(mktemp); \
	./main < test_input.txt | tee $$TMP ; \
	if cmp $$TMP test_output.txt; \
	then echo "PASS:"; \
	else echo "FAIL - Expected:"; cat test_output.txt; \
	fi; \
	rm -f $$TMP

clean :
	rm -f main main.o main.hi
