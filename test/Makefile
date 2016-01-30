all: Tests.hs
	ghc -O Tests -ddump-simpl -ddump-to-file

play:
	hermit Tests.hs -v0 -opt=ShapedTypes.Interactive Play.hss
