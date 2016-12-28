dist/hipio:
	mkdir -p dist
	stack \
		--jobs `cat /proc/cpuinfo | grep processor | wc -l` \
		--local-bin-path dist \
		install \
		--ghc-options '-fPIC' \
		--split-objs

dist: dist/hipio
	upx -q --best --ultra-brute dist/hipio

clean:
	rm -rf dist

.PHONY: clean dist
