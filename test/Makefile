default: Tests

Tests: ../dist Tests.hs Makefile
	time ghc Tests.hs -fforce-recomp -fplugin=ShapedTypes.Plugin -O -dcore-lint # -dverbose-core2core

play:
	hermit Tests.hs -v0 -opt=ShapedTypes.Interactive Play.hss

compile:
	cd .. ; make

circat:
	cd ../../circat ; make
	make mono

extras:
	cd ../../hermit-extras ; make
	make mono

mono:
	cd ../../monomorph ; make
	make lam

lam:
	cd ../../lambda-ccc ; make
	make compile

unregister-all:
	@echo "Unregistering:"
	@ghc-pkg unregister shaped-types  --force || true
	@ghc-pkg unregister lambda-ccc    --force || true
	@ghc-pkg unregister monomorph     --force || true
	@ghc-pkg unregister hermit-extras --force || true
	@ghc-pkg unregister circat        --force || true

recompile-all: unregister-all
	cd ../../circat	       ; cabal clean && cabal install
	cd ../../hermit-extras ; cabal clean && cabal install
	cd ../../monomorph     ; cabal clean && cabal install
	cd ../../lambda-ccc    ; cabal clean && cabal install
	cd ..		       ; cabal clean && cabal install
