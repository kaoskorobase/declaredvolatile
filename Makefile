index.html: index.hs AsciiArt.hs
	runhaskell index.hs > index.html

.PHONY: deploy
deploy: index.html
	ssh null2.net "sh -c 'cd /home/n222/html/sk/sites/space && git pull'"
