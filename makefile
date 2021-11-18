build:
	nix-build

server:
	cd result && python3 -m http.server

dev:
	nix-shell --run 'JSADDLE_WARP_PORT=8000 ghcid -T :main'

pids:
	lsof -i tcp:8000 | awk '{print $$2}'