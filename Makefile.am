ACLOCAL_AMFLAGS=-I ac-macros

BUILT_SOURCES= src/RawFiles.hs src/FrontEnd/HsParser.hs src/FlagDump.hs \
	       src/FlagOpts.hs src/Name/Prim.hs src/Info/Properties.hs

MY_CINCLUDES= -I$(srcdir)/src/data -I$(srcdir)/src/StringTable -I$(srcdir)/src/cbits -I$(builddir)/src/cbits

dist-hook: $(BUILT_SOURCES)
	if test -f .git; then \
		rm -f $(srcdir)/ChangeLog $(srcdir)/src/data/shortchange.txt; \
		$(MAKE) ChangeLog src/data/shortchange.txt src/RawFiles.hs;   \
		cp -f $(srcdir)/ChangeLog $(destdir)/ChangeLog; \
		cp -f $(srcdir)/src/data/shortchange.txt $(destdir)/src/data/shortchange.txt; \
		cp -f $(srcdir)/src/RawFiles.hs $(destdir)/src/RawFiles.hs; \
	fi

all-local: cabal-build
install-data-hook: cabal-install

RAWFILES = src/data/ViaGhc.hs src/data/prelude.m4 \
	   src/data/targets.ini rts/rts/constants.h rts/rts/stableptr.c rts/rts/stableptr.h rts/sys/queue.h rts/HsFFI.h \
           rts/sys/wsize.h rts/sys/bitarray.h ChangeLog src/data/shortchange.txt rts/rts/slub.c \
	   rts/rts/gc_jgc.c rts/rts/gc_jgc.h rts/rts/profile.c rts/rts/profile.h rts/rts/cdefs.h rts/rts/rts_support.c \
	   rts/rts/rts_support.h rts/rts/gc.h rts/rts/gc_none.c rts/rts/gc_none.h rts/rts/jhc_rts.c rts/rts/jhc_rts.h \
	   rts/lib/lib_cbits.c rts/jhc_rts_header.h rts/lib/lib_cbits.h rts/rts/gc_jgc_internal.h \
	   rts/rts/conc.c rts/rts/conc.h

install-exec-hook: cabal-install

# Various rules for generated Haskell files
%.hs: %.hsc
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	"$(HSC2HS)" $(MY_CINCLUDES) -o $@ $<

%.hs: %.flags utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $< > $@

%.mkd: %.flags utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -f f -m -n $< $< > $@

src/Info/Properties.hs: src/data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@

src/Name/Prim.hs: utils/op_names.prl src/data/names.txt
	perl $< > $@ || rm -f $@

src/RawFiles.hs: $(RAWFILES)
	perl $(srcdir)/utils/op_raw.prl $(basename $@) $^ > $@

src/FrontEnd/HsParser.hs: src/FrontEnd/HsParser.y
	happy -a -g -c $< -o $@

.SECONDARY: src/data/shortchange.txt

ChangeLog:
	git log fork-jhc-0.8.0-darcs20120314.. > $@

src/data/shortchange.txt: ChangeLog
	echo -n "`head -1 $< | sed -ne 's/^commit \(.*\)/\1/p'`"  > $@

# cabal wrapper
cabal-dist: $(BUILT_SOURCES) src/StringTable/Atom.hs 
	runhaskell Setup.hs sdist

cabal-build: $(BUILT_SOURCES) src/StringTable/Atom.hs
	cabal configure
	cabal build

cabal-setup: $(BUILT_SOURCES) src/StringTable/Atom.hs

cabal-install:
	cabal install --disable-documentation

.INTERMEDIATE: ajhc_man.mkd options.mkd
.DELETE_ON_ERROR: ChangeLog data/shortchange.txt manual.mkd ajhc_man.mkd options.mkd

lint:
	perl utils/hslint.prl `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`
hlint:
	hlint `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`

ho-clean:
	rm -f -- `find -name \*.ho -print`
	rm -f -- `find ~/.ajhc/cache -name \*.ho -print`
hl-clean:
	rm -f -- `find -name \*.hl -print`

clean-local:
	cabal clean
	find . \( \! -path \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) \) -print | xargs rm -f --
	rm -rf dist tmp autom4te.cache
	$(MAKE) -C selftest clean
	$(MAKE) -C rts/test clean

CLEANFILES= $(JHC_LIBS) $(JHC_EXT_LIBS) HelloWorld Primes src/StringTable/Atom.hs src/RawFiles.hs
CLEANFILES+= ajhc.spec manual.html manual.mkd manual_ja.html manual_ja.mkd docs/building.mkd src/FlagDump.mkd src/FlagOpts.mkd
CLEANFILES+= src/FlagDump.hs src/FlagOpts.hs src/FrontEnd/HsParser.hs src/Info/Properties.hs src/Name/Prim.hs
CLEANFILES+= src/data/shortchange.txt ChangeLog ajhc.1 *.hl

# docs
%.pdf: %.ps
	epstopdf $< --outfile $@

%.ps : %.dot
	dot $< -Tps -o$@

%.pdf : %.dot
	dot $< -Tpdf -o$@

%.shtml: %.mkd
	cat $(srcdir)/docs/template/my_header.shtml > $@
	pandoc $< -f markdown -t html >> $@
	cat $(srcdir)/docs/template/my_footer.shtml >> $@

publish: docs/building.shtml docs/big-picture.pdf docs/development.shtml docs/index.shtml docs/ajhc.shtml manual.html docs/manual.css
	cp -- $^ /home/john/public_html/computer/ajhc

publish_ajhc: manual.html manual_ja.html docs/manual.css
	cp -- $^ /home/kiwamu/doc/ajhc.github.com/

publish_haddock: $(BUILT_SOURCES) src/StringTable/Atom.hs
	cabal configure
	cabal haddock --hyperlink-source --html
	rm -rf /home/kiwamu/doc/ajhc.github.com/api
	cp -r dist/doc/html/ajhc /home/kiwamu/doc/ajhc.github.com/api

manual.mkd: $(JHC_MANUAL_FILES)
	perl $(srcdir)/utils/stitch.prl $^ > $@

%.html: %.mkd
	pandoc $< --toc -s -f markdown -t html -s -c manual.css -o $@

updatepo: manual.mkd
	po4a-updatepo -M utf8 -f text -m manual.mkd -p po/ja.po

manual_ja.mkd: manual.mkd po/ja.po
	po4a-translate -M utf8 -f text -m manual.mkd -p po/ja.po -l manual_ja.mkd

$(firstword $(JHC_MANUAL_FILES)): utils/stitch.prl docs/man_header.mkd
	touch $@

# man pages
ajhc_man.mkd: $(JHC_MANUAL_FILES)
	perl $(srcdir)/utils/stitch.prl -h $(srcdir)/docs/man_header.mkd -c Using,Options $^ > $@

ajhc.1: ajhc_man.mkd
	pandoc $< -s -f markdown -t man -s  -o $@

options.mkd: src/Options.hs cabal-build
	echo "{-@Options 1" > $@
	./dist/build/ajhc/ajhc --help | sed -e 's/^/    /'  >> $@

fix-whitespace:
	find src -not \( -name .svn -prune -o -name .git -prune \) -type f -name \*.hs -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'
	find lib -not \( -name .svn -prune -o -name .git -prune \) -type f -name \*.hs -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'

show_elibs: lib/ext/*.cabal  lib/ext/*.yaml
	-perl utils/build_extlibs.prl -l $^

JHC_MANUAL_FILES = docs/make.mkd options.mkd docs/dependency_format.mkd docs/pragmas.mkd docs/differences.mkd \
	docs/unboxed.mkd rts/rts/jhc_rts.c src/FlagOpts.mkd src/FlagDump.mkd src/E/PrimOpt.hs \
	src/E/TypeCheck.hs src/Options.hs

regress: cabal-build
	mkdir -p regress/results/dummy
	ln -sf regress/results/dummy regress/results/current
	ln -sf regress/results/dummy regress/results/late
	ln -sf regress/results/dummy regress/results/latest
	./regress/regress.prl

selftest: $(BUILT_SOURCES) src/StringTable/Atom.hs
	$(MAKE) -C selftest test

rtstest:
	$(MAKE) -C rts/test test

.PHONY: regress selftest rtstest ho-clean hl-clean updatepo cabal-dist cabal-build cabal-install
