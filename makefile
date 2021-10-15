build:
	nix-build

deploy:
	cp -r result ../tmp && git checkout gh-pages && cp ../tmp/* ./ && git add . && git commit -m "update" && git push && rm -rf ../tmp