.PHONY: all
all: generated/Spotify/Api.elm

build/spotify-web-api-releases.json:
	mkdir -p build
	curl -H "Accept: application/vnd.github+json" -H "X-GitHub-Api-Version: 2022-11-28" https://api.github.com/repos/sonallux/spotify-web-api/releases > $@

build/spotify-web-api-original.yaml: build/spotify-web-api-releases.json
	curl -sSL $$(jq -r '.[0].assets[] | select(.name == "fixed-spotify-open-api.yml").browser_download_url' < $<) > $@

generated/Spotify/Api.elm: build/spotify-web-api-spaceless.yaml Makefile
	yarn elm-open-api --module-name Spotify build/spotify-web-api-spaceless.yaml

build/spotify-web-api-fixed.yaml: build/spotify-web-api-original.yaml src/api-patch.diff Makefile
	cp $< $@
	patch $@ src/api-patch.diff

build/spotify-web-api-spaceless.yaml: build/spotify-web-api-fixed.yaml
	grep -v '^$$' < $< > $@

.PHONY: gen-patch
gen-patch:
	diff -u build/spotify-web-api-original.yaml build/spotify-web-api-fixed.yaml > src/api-patch.diff --label build/spotify-web-api-original.yaml --label build/spotify-web-api-fixed.yaml || true
