main : main.hs
	ghc -Wall -Wextra main.hs

test : main input.txt
	./main < input.txt

clean :
	rm -f main main.o main.hi
