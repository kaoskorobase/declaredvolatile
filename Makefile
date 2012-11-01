index.html: AsciiArt.hs index.hs
	runhaskell index.hs > index.html

.PHONY: deploy
deploy:
	sh -x deploy.sh
