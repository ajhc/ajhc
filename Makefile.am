
ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = Main.hs  $(HSFILES) $(BUILT_SOURCES) $(HSBOOT)

SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot
BUILT_SOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs FlagOpts.hs Version/Raw.hs Version/Ctx.hs Name/Prim.hs Info/Properties.hs

GHCDEBUGOPTS= -W -fno-warn-unused-matches  -fwarn-type-defaults
GHCPROFOPTS=   -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCINC=  -i$(srcdir)/. -i$(srcdir)/FrontEnd -odir $(builddir) -hidir $(builddir)
PACKAGES= -package mtl  -package unix -package QuickCheck -ignore-package lang
GHCLANG=    -fallow-undecidable-instances  -fglasgow-exts -fallow-overlapping-instances
GHCOPTS=  @HSOPTS@  -O -ignore-package lang  -pgmF drift-ghc  -F $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) $(GHCLANG)

HC = ghc
HCI = ghci
HC_OPTS = $(GHCOPTS)
PROF_OPTS = -P

EXTRA_DIST = FrontEnd/HsParser.y Version/Ctx.hs Version/Raw.hs $(RAWFILES) depend.make  arch/generic.arch arch/arch.c arch/i686.arch arch/x86_64.arch

RAWFILES = $(srcdir)/data/HsFFI.h $(srcdir)/data/jhc_rts.c $(srcdir)/data/jhc_rts_header.h $(srcdir)/data/wsize.h $(srcdir)/data/jhc_rts_alloc.c $(srcdir)/data/jhc_rts2.c $(srcdir)/data/ViaGhc.hs

-include depend.make

jhc: Main.hs  $(HSFILES) $(BUILT_SOURCES)
	$(HC) $(GHCOPTS) --make $< -o $@

HSBOOT= DataConstructors.hs-boot FrontEnd/Tc/Class.hs-boot Grin/Grin.hs-boot Grin/Show.hs-boot Info/Binary.hs-boot

# Various rules for generated Haskell files


%.hs: %.hsc
	$(HSC2HS) -C "$(CFLAGS)" -C -I. -C -I@srcdir@ -o $@ $<

%.hs: %.flags  $(srcdir)/utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $<  > $@

Info/Properties.hs: $(srcdir)/data/props.txt $(srcdir)/utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@


PrimitiveOperators.hs: $(srcdir)/utils/op_process.prl $(srcdir)/data/operators.txt $(srcdir)/data/primitives.txt $(srcdir)/data/PrimitiveOperators-in.hs
	perl $(srcdir)/utils/op_process.prl > $@ || rm -f $@

Name/Prim.hs: $(srcdir)/utils/op_names.prl $(srcdir)/data/primitives.txt $(srcdir)/data/names.txt
	perl $(srcdir)/utils/op_names.prl > $@ || rm -f $@

RawFiles.hs:  $(RAWFILES)
	perl $(srcdir)/utils/op_raw.prl $(basename $@)  $^ > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.y
	happy -a -g -c $<

.INTERMEDIATE: changes.txt

_dacs/inventory:
	touch $@

changes.txt: $(srcdir)/_darcs/inventory
	(darcs changes --context || echo "No darcs Context Available!") > $@

Version/Ctx.hs: changes.txt
	rm -f $@
	perl ./utils/op_raw.prl Version.Ctx $< > $@

Version/Raw.hs: $(srcdir)/_darcs/inventory
	rm -f $@
	echo "module Version.Raw where"                                    > $@
	echo "jhcVersion = \"@VERSION@\""                                  >> $@
	date +'compileDate = "%Y%m%d"'                                     >> $@
	darcs changes -t '.' \
	|  perl -e '<>;$$_=<>;s/^\s*tagged\s+/darcsTag = "/;s/$$/"/;print' >> $@
	darcs changes --from-tag='' --xml-output | grep '</patch>' \
	| wc -l | perl -e 'print "darcsPatches = \"".(<>-1)."\"\n"'        >> $@
	echo '{-# NOINLINE libraryPath #-}'                                >> $@
	echo 'libraryPath=["$(libdir)/jhc-@VERSION@"]'                     >> $@


BINDISTDIR=$(PACKAGE)-$(VERSION)-$(host)

bin-dist: jhc
	rm -rf -- $(BINDISTDIR)
	mkdir $(BINDISTDIR)
	strip -- jhc || true
	cp -- jhc $(BINDISTDIR)
	tar cvf $(BINDISTDIR).tar $(BINDISTDIR)
	gzip -f -- $(BINDISTDIR).tar
	rm -rf -- $(BINDISTDIR)

rpm: jhc.spec dist
	cp jhc-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/
	rpmbuild -ba jhc.spec

# .PHONY: bin-dist depend clean
