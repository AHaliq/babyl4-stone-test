build:
	nix-build

deploy:
	rm -rf ../tmp && mkdir -p ../tmp && cp -r ./result/* ../tmp && cp -r ./static/* ../tmp && git checkout gh-pages && cp -r ../tmp/* ./ && git add . && git commit -m "update" && git push && rm -rf ../tmp

server:
	rm -rf ../tmp && mkdir -p ../tmp && cp -r ./result/* ../tmp && cp -r ./static/* ../tmp && cd ../tmp && python3 -m http.server