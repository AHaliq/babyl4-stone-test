build:
	nix-build

deploy:
	mkdir ../tmp && cp -r ./result/* ../tmp && git checkout gh-pages && cp -r ../tmp/* ./ && git add . && git commit -m "update" && git push && rm -rf ../tmp