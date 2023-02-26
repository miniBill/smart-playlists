.PHONY: all
all: generated/Api.elm

build/spotify-web-api-releases.json:
	mkdir -p build
	curl -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/sonallux/spotify-web-api/releases > $@

build/spotify-web-api.yaml: build/spotify-web-api-releases.json
	curl -sSL $$(jq -r '.[0].assets[] | select(.name == "fixed-spotify-open-api.yml").browser_download_url' < $^) > $@

build/spotify-web-api.json: build/spotify-web-api.yaml
	nix-shell -p yq --run 'yq "." $^' > $@

GENERATOR_PATH = ../elm-api-sdk-generator

generated/Api.elm: build/spotify-web-api.json
	(export ORIGIN=$$(realpath --relative-to ${GENERATOR_PATH} $$(pwd)); cd ${GENERATOR_PATH}; npm run dev $$ORIGIN/build/spotify-web-api.json -- --output $$ORIGIN/generated/Api.elm)
