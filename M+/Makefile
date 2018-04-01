all:
	happy -gca ParM.y
	alex -g LexM.x
	ghc --make -XDeriveGeneric Interpret.hs -o Interpret

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi

	

