install:
	cabal update
	cabal install

clean:
	cabal clean

run:
	@./dist/build/haskell-go-checkers/haskell-go-checkers
