.PHONY: all
all: build/spotify-web-api.json

build/spotify-web-api-releases.json:
	mkdir -p build
	curl -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/sonallux/spotify-web-api/releases > $@

build/spotify-web-api.yaml: build/spotify-web-api-releases.json
	curl -sSL $$(jq -r '.[0].assets[] | select(.name == "fixed-spotify-open-api.yml").browser_download_url' < $^) > $@

build/spotify-web-api.json: build/spotify-web-api.yaml
	nix-shell -p yq --run 'yq "." $^' > $@
