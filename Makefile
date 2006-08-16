JHC_VERSION=0.1
PREFIX=/usr/local

all: jhc

GHCDEBUGOPTS= -W -fno-warn-unused-matches -fno-warn-unused-binds    # -O2 -ddump-simpl-stats -ddump-rules
GHCPROFOPTS=   -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCINC=  -iFrontEnd
PACKAGES= -package mtl  -package unix -package QuickCheck
GHCOPTS=   -O -ignore-package lang  -pgmF drift-ghc  -F $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) -fwarn-type-defaults   -fallow-undecidable-instances  -fglasgow-exts -fallow-overlapping-instances

HC = ghc
HCI = ghci
HC_OPTS = $(GHCOPTS)

PROF_OPTS = -P

BUILTSOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs FlagOpts.hs Version/Raw.hs Version/Ctx.hs Name/Prim.hs

# HSFILES is defined here, it can be updated with 'make depend' whenever a new source file is added
-include depend.make


SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot


MAIN=Main.hs

jhcp: $(BUILTSOURCES) $(HSFILES)
	date '+%y%m%d%H%M.%S' > /tmp/$@.date.tmp
	$(HC) $(GHCOPTS) $(EXTRAOPTS) $(GHCPROFOPTS) --make $(MAIN) -o $@
	touch -t `cat /tmp/$@.date.tmp` $@

jhc: $(BUILTSOURCES) $(HSFILES)
	date '+%y%m%d%H%M.%S' > /tmp/$@.date.tmp
	$(HC) $(GHCOPTS) $(EXTRAOPTS) --make $(MAIN) -o $@
	touch -t `cat /tmp/$@.date.tmp` $@

i:
	ghci $(GHCOPTS) $(EXTRAOPTS) Main.hs

LIBPACKAGES=base-1.0.hl haskell98-1.0.hl
LIBRARYPATH="$(PREFIX)/lib/jhc-$(JHC_VERSION)"
DP=$(PREFIX)

base-1.0.hl: jhc lib/base/base.cabal
	-[ -e base.log ] && mv -f base.log base.log.bak
	set -o pipefail; ./jhc -v $(RTSOPTS) $(JHC_TEST)  -ilib/base --noauto --build-hl lib/base/base.cabal -o $@ 2>&1 | tee base.log

base-1.0.prof.hl: jhc lib/base/base.cabal
	-[ -e base.prof.log ] && mv -f base.prof.log base.prof.log.bak
	./jhcp -v $(RTSOPTS) $(JHC_TEST) -ilib/base --noauto --build-hl lib/base/base.cabal -o base-1.0.prof.hl +RTS $(PROF_OPTS)  2>&1 | tee base.log

libs: $(LIBPACKAGES)

publish: $(LIBPACKAGES)
	cp -f $(LIBPACKAGES) ~/public_html/computer/jhc/libs
	make -C ~/public_html/computer/jhc

fetch-libs:
	rm -f $(LIBPACKAGES)
	for f in $(LIBPACKAGES); do wget http://repetae.net/john/computer/jhc/libs/$${f}; done
	touch $(LIBPACKAGES)

haskell98-1.0.hl: jhc lib/haskell98/haskell98.cabal base-1.0.hl
	./jhc -v $(RTSOPTS) $(JHC_TEST) -ilib/haskell98 --noauto -L- -L. -p base --build-hl lib/haskell98.cabal -o $@

QuickCheck-1.0.hl: jhc base-1.0.hl
	./jhc -v $(RTSOPTS) -d progress $(JHC_TEST) -ilib/QuickCheck -L- -L. -f cpp --build-hl lib/QuickCheck/QuickCheck.cabal -o $@

install:
	install -d "$(DP)/bin"
	install jhc "$(DP)/bin"
	ln -sf "$(DP)/bin/jhc" "$(DP)/bin/jhci"
	install -d $(LIBRARYPATH)
	install $(LIBPACKAGES) $(LIBRARYPATH)

tags:
	hasktags -c `find . -type f -name '*hs' | egrep -v '^\./(_darcs|lib|test)/'`
	mv tags tags.tmp
	LC_ALL=C sort tags.tmp > tags
	rm tags.tmp


hsdocs:
	haddock -h $(filter-out DataConstructors.hs SelfTest.hs %/HsParser.hs FrontEnd/Representation.hs C/Gen.hs , $(HSFILES)) -o hsdocs

printos:
	echo $(HSFILES)


depend: $(BUILTSOURCES)
	$(HC) -M -optdep-f -optdepdep.tmp $(HC_OPTS) $(MAIN)
	echo HSFILES=`egrep -o '[A-Za-z0-9/.]+.hs' dep.tmp | sed -e 's/^\.\///' | sort` > depend.make
	rm -f dep.tmp

clean:
	rm -f  jhc jhcp *.hs_code.c `find . -name \*.hi -or -name \*.o-boot -or -name \*.hi-boot -or -name \*.o`

tests: helloworld calendar primes

helloworld: test/HelloWorld.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v $(JHC_TEST) test/HelloWorld.hs -o $@ 2>&1 | tee $@.log
calendar: test/Calendar.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v $(JHC_TEST) test/Calendar.hs -o $@ 2>&1 | tee $@.log
primes: test/Primes.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v $(JHC_TEST) test/Primes.hs -o $@ 2>&1 | tee $@.log

realclean: clean
	rm -f $(BUILTSOURCES) depend.make

builtfiles: $(BUILTSOURCES)

clean-ho:
	rm -f -- `find -name \*.ho`


# Various rules for generated Haskell files

%.hs: %.flags  ./utils/opt_sets.prl
	perl ./utils/opt_sets.prl -n $< $<  > $@

PrimitiveOperators.hs: utils/op_process.prl data/operators.txt data/primitives.txt data/PrimitiveOperators-in.hs
	perl ./utils/op_process.prl > $@ || rm -f $@

Name/Prim.hs: utils/op_names.prl data/primitives.txt
	perl ./utils/op_names.prl > $@ || rm -f $@

RawFiles.hs:  data/HsFFI.h data/jhc_rts.c
	perl ./utils/op_raw.prl $(basename $@)  $^ > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.ly
	happy -a -g -c FrontEnd/HsParser.ly

Version/Ctx.hs: _darcs/inventory
	rm -f $@
	darcs changes --context > changes.txt  || echo "No darcs Context Available!" > changes.txt
	perl ./utils/op_raw.prl Version.Ctx changes.txt > $@
	rm -f changes.txt

Version/Raw.hs: _darcs/inventory
	rm -f $@
	echo "module Version.Raw where"                                > $@
	echo "jhcVersion = \"$(JHC_VERSION)\""                         >> $@
	date +'compileDate = "%Y%m%d"'                                 >> $@
	darcs changes -t '.' \
	|  perl -e '<>;$$_=<>;s/^\s*tagged\s+/darcsTag = "/;s/$$/"/;print' >> $@
	darcs changes --from-tag='' --xml-output | grep '</patch>' \
	| wc -l | perl -e 'print "darcsPatches = \"".(<>-1)."\"\n"'    >> $@
	echo '{-# NOINLINE libraryPath #-}'                            >> $@
	echo 'libraryPath=["$(PREFIX)/lib/jhc-$(JHC_VERSION)"]'        >> $@

.PHONY: depend clean realclean builtfiles clean-ho  regress hsdocs install i printos tests libs publish tags

