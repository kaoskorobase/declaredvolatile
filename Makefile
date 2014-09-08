index.html: index.hs AsciiArt.hs
	cabal exec runhaskell index.hs > index.html

.PHONY: deploy
deploy:
	sh -x deploy.sh

