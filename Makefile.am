
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
GHCOPTS=  $(HSOPTS)  -O -ignore-package lang  -pgmF drift-ghc  -F $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) $(GHCLANG)

HC = ghc
HCI = ghci
HC_OPTS = $(GHCOPTS)
PROF_OPTS = -P

EXTRA_DIST = FrontEnd/HsParser.y Version/Ctx.hs Version/Raw.hs $(DATAFILES) $(UTILS) $(RAWFILES) depend.make  arch/generic.arch arch/arch.c arch/i686.arch arch/x86_64.arch  data/PrimitiveOperators-in.hs

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = data/HsFFI.h data/jhc_rts.c data/jhc_rts_header.h data/wsize.h data/jhc_rts_alloc.c data/jhc_rts2.c data/ViaGhc.hs

DATAFILES = data/operators.txt data/primitives.txt  data/names.txt data/props.txt

-include depend.make

jhc: Main.hs  $(HSFILES) $(BUILT_SOURCES)
	$(HC) $(GHCOPTS) --make $< -o $@

HSBOOT= DataConstructors.hs-boot FrontEnd/Tc/Class.hs-boot Grin/Grin.hs-boot Grin/Show.hs-boot Info/Binary.hs-boot

# Various rules for generated Haskell files


%.hs: %.hsc
	$(HSC2HS) -C "$(CFLAGS)" -C -I. -C -I@srcdir@ -o $@ $<

%.hs: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $<  > $@

Info/Properties.hs: data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@


PrimitiveOperators.hs: utils/op_process.prl data/operators.txt data/primitives.txt data/PrimitiveOperators-in.hs
	perl $< > $@ || rm -f $@

Name/Prim.hs: utils/op_names.prl data/primitives.txt data/names.txt
	perl $< > $@ || rm -f $@

RawFiles.hs:  utils/op_raw.prl $(RAWFILES)
	perl $< $(basename $@)  $(RAWFILES) > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.y
	happy -a -g -c $<

.INTERMEDIATE: changes.txt

.SECONDARY: _darcs/inventory

changes.txt: _darcs/inventory
	(darcs changes --context || echo "No darcs Context Available!") > $@

Version/Ctx.hs: changes.txt
	rm -f $@
	perl ./utils/op_raw.prl Version.Ctx $< > $@

Version/Raw.hs: _darcs/inventory
	rm -f $@
	echo "module Version.Raw where"                                    > $@
	echo "jhcVersion = \"@VERSION@\""                                  >> $@
	date +'compileDate = "%Y-%m-%d"'                                   >> $@
	darcs changes -t '.' \
	|  perl -e '<>;$$_=<>;s/^\s*tagged\s+/darcsTag = "/;s/$$/"/;print' >> $@
	darcs changes --from-tag='' --xml-output | grep '</patch>' \
	| wc -l | perl -e 'print "darcsPatches = \"".(<>-1)."\"\n"'        >> $@
	echo 'libdir="$(libdir)"'                                          >> $@
	echo 'host="$(host)"'                                              >> $@
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
	cp jhc-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/jhc-$(VERSION).tar.gz
	rpmbuild -ba jhc.spec

# .PHONY: bin-dist depend clean

.INTERMEDIATE: deps.txt
.DELETE_ON_ERROR: deps.txt depend.make

deps.txt: Main.hs $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(HC_OPTS) $<

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/.]+.hs' $< | sed -e 's/^\.\///' | sort` > $@


