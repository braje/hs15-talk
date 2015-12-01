all: index.html

PANDOC=pandoc\
		--from=markdown \
		-t revealjs \
		--section-divs \
		--filter ./filter \
		-H header.tmpl \
		--smart -s

index.html: hs15.markdown src/Common.hs src/ReflexCommon.hs filter-pandoc
	$(PANDOC) hs15.markdown -o index.html

filter-pandoc: pandoc-filter/filter.hs
	cd pandoc-filter && stack build && cd ..
	cp `find ./pandoc-filter/.stack-work/install -type f -name filter` .

.PHONY: clean

clean:
	rm -rf *~ src/M* src/main.* src/*.js_* filter
