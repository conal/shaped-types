default: Tests

Tests: ../dist Tests.hs Makefile
	time ghc Tests.hs -O -dcore-lint -fforce-recomp -fplugin=LambdaCCC.Plugin -fplugin-opt=LambdaCCC.Reify:verbose

# It's odd to use LambdaCCC.Reify here, but when I use the plugin module name,
# our plugin never gets run. Same for the interactive plugin below.

# -dverbose-core2core

play:
	hermit Tests.hs -v0 -opt=LambdaCCC.Interactive Play.hss -- -fplugin-opt=LambdaCCC.Reify:verbose

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
