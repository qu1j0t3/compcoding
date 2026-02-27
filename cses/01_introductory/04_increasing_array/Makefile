main : main.hs
	ghc -Wall main.hs

test : main input.txt
	./main < input.txt

clean :
	rm -f main main.o main.hi
