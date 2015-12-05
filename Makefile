all: index.html

PANDOC=pandoc\
		--from=markdown \
		-t revealjs \
		--section-divs \
		--filter ./filter \
		-H header.tmpl \
		--smart -s

index.html: hs15.markdown src/Common.hs src/CommonReflex.hs filter-pandoc
	$(PANDOC) hs15.markdown -o index.html

filter-pandoc: pandoc-filter/filter.hs
	cd pandoc-filter && stack build && cp `stack exec which -- filter` .. && cd ..

.PHONY: clean

clean:
	rm -rf *~ src/M* src/R*.hs src/main.* src/*.js_* filter reveal.js/css/theme/*.css index.html
	git submodule foreach git reset --hard
