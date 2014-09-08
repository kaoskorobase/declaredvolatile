index.html: index.hs AsciiArt.hs
	cabal exec runhaskell index.hs > index.html
