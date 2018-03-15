install:
	alex Tokens.x
	happy Grammar.y -o Grammar.hs
	ghc -XRankNTypes -o myinterpreter Main.hs
