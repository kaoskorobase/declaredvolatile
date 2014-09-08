index.html: index.hs AsciiArt.hs
	cabal exec runhaskell index.hs > index.html

.PHONY: deploy
deploy:
	test -x deploy.sh && sh -x deploy.sh || true

