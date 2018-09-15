run: clean
	stack build --fast && stack exec gen-docs ghc/compiler

clean:
	rm -r docs

