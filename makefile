build:
	nix-build

server:
	cd result && python3 -m http.server