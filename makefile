install:
	alex Tokens.x
	happy Grammar.y -o Grammar.hs
	ghc -o Main Main.hs
