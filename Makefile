.PHONY: all
all: generated/Api.elm

build/spotify-web-api-releases.json:
	mkdir -p build
	curl -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/sonallux/spotify-web-api/releases > $@

build/spotify-web-api-original.yaml: build/spotify-web-api-releases.json
	curl -sSL $$(jq -r '.[0].assets[] | select(.name == "fixed-spotify-open-api.yml").browser_download_url' < $<) > $@

GENERATOR_PATH = ../elm-api-sdk-generator

generated/Api.elm: build/spotify-web-api-spaceless.yaml ${GENERATOR_PATH}/script/src/Cli.elm $(shell find ${GENERATOR_PATH}/src -type f -name '*.elm') Makefile
	(export ORIGIN=$$(realpath --relative-to ${GENERATOR_PATH} $$(pwd)); cd ${GENERATOR_PATH}; npm run dev $$ORIGIN/build/spotify-web-api-spaceless.yaml -- --output $$ORIGIN/generated/Api.elm)
	elm-format --yes $@

build/spotify-web-api-fixed.yaml: build/spotify-web-api-original.yaml src/api-patch.diff Makefile
	cp $< $@
	patch $@ src/api-patch.diff

build/spotify-web-api-spaceless.yaml: build/spotify-web-api-fixed.yaml
	grep -v '^$$' < $< > $@

.PHONY: gen-patch
gen-patch:
	diff -u build/spotify-web-api-original.yaml build/spotify-web-api-fixed.yaml > src/api-patch.diff --label build/spotify-web-api-original.yaml --label build/spotify-web-api-fixed.yaml || true
