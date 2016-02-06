# include ../my-cabal-make.inc

install:
	time cabal install -j1 --disable-documentation --force-reinstalls

fresh:
	cabal clean && make install
