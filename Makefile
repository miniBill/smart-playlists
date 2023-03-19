.PHONY: all
all: generated/Api.elm

build/spotify-web-api-releases.json:
	mkdir -p build
	curl -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/sonallux/spotify-web-api/releases > $@

build/spotify-web-api-original.yaml: build/spotify-web-api-releases.json
	curl -sSL $$(jq -r '.[0].assets[] | select(.name == "fixed-spotify-open-api.yml").browser_download_url' < $<) > $@

GENERATOR_PATH = ../elm-api-sdk-generator

generated/Api.elm: build/spotify-web-api-fixed.yaml ${GENERATOR_PATH}/script/src/Cli.elm ${GENERATOR_PATH}/script/elm.json ${GENERATOR_PATH}/script/src/CliMonad.elm ${GENERATOR_PATH}/script/src/Common.elm Makefile
	(export ORIGIN=$$(realpath --relative-to ${GENERATOR_PATH} $$(pwd)); cd ${GENERATOR_PATH}; npm run dev $$ORIGIN/build/spotify-web-api-fixed.yaml -- --output $$ORIGIN/generated/Api.elm)
	elm-format --yes $@

build/spotify-web-api-fixed.yaml: build/spotify-web-api-original.yaml src/api-patch.diff Makefile
	grep -ve '^$$' < $< > $@
	patch $@ src/api-patch.diff

.PHONY: gen-patch
gen-patch:
	diff -u build/spotify-web-api-original.yaml build/spotify-web-api-fixed.yaml > src/api-patch.diff --label build/spotify-web-api-original.yaml --label build/spotify-web-api-fixed.yaml || true
