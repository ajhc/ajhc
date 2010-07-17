
ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = src/Main.hs  $(HSFILES) $(BUILT_SOURCES) $(HSBOOT) $(DRIFTFILES)

BUILT_SOURCES= src/PrimitiveOperators.hs src/RtsFiles.hs src/RawFiles.hs src/FrontEnd/HsParser.hs src/FlagDump.hs \
	       src/FlagOpts.hs src/Name/Prim.hs src/Info/Properties.hs $(DRIFTFILES)

MY_CINCLUDES= -I$(srcdir)/src/data -I$(srcdir)/src/StringTable -I$(srcdir)/src/cbits -I$(builddir)/src/cbits
MY_CFLAGS= $(MY_CINCLUDES) -optc-std=c99 -optc-O2 -optc-Wall

HSFILES=src/C/Generate.hs src/Cmm/Number.hs src/Cmm/OpEval.hs src/DerivingDrift/DataP.hs \
	src/DerivingDrift/Drift.hs src/DerivingDrift/RuleUtils.hs src/Doc/Attr.hs src/Doc/Chars.hs src/Doc/DocLike.hs \
	src/Doc/PPrint.hs src/Doc/Pretty.hs src/E/Annotate.hs src/E/Binary.hs src/E/Diff.hs src/E/E.hs src/E/Eta.hs src/E/Eval.hs \
	src/E/FreeVars.hs src/E/FromHs.hs src/E/Inline.hs src/E/LetFloat.hs src/E/PrimOpt.hs src/E/Program.hs src/E/Rules.hs \
	src/E/Show.hs src/E/Subst.hs src/E/Traverse.hs src/E/TypeAnalysis.hs src/E/Values.hs src/E/WorkerWrapper.hs \
	src/FindFixpoint.hs src/Fixer/Fixer.hs src/Fixer/Supply.hs src/Fixer/VMap.hs src/FlagDump.hs src/FlagOpts.hs \
	src/FrontEnd/DataConsAssump.hs src/FrontEnd/DeclsDepends.hs src/FrontEnd/DependAnalysis.hs src/FrontEnd/Desugar.hs \
	src/FrontEnd/Diagnostic.hs src/FrontEnd/FrontEnd.hs src/FrontEnd/HsErrors.hs src/FrontEnd/HsParser.hs \
	src/FrontEnd/HsPretty.hs src/FrontEnd/Infix.hs src/FrontEnd/Lexer.hs src/FrontEnd/ParseMonad.hs src/FrontEnd/ParseUtils.hs \
	src/FrontEnd/Rename.hs src/FrontEnd/Syn/Options.hs src/FrontEnd/Syn/Traverse.hs src/FrontEnd/Tc/Class.hs src/FrontEnd/Tc/Main.hs \
	src/FrontEnd/Tc/Module.hs src/FrontEnd/Tc/Type.hs src/FrontEnd/Tc/Unify.hs src/FrontEnd/TypeSigs.hs src/FrontEnd/TypeSynonyms.hs\
       	src/FrontEnd/TypeSyns.hs src/FrontEnd/Unlit.hs src/FrontEnd/Utils.hs src/FrontEnd/Warning.hs src/GenUtil.hs src/Grin/DeadCode.hs\
       	src/Grin/Devolve.hs src/Grin/EvalInline.hs src/Grin/FromE.hs src/Grin/Grin.hs src/Grin/HashConst.hs src/Grin/Lint.hs\
       	src/Grin/NodeAnalyze.hs src/Grin/Noodle.hs src/Grin/Optimize.hs src/Grin/Show.hs src/Grin/Simplify.hs src/Grin/Val.hs \
	src/Grin/Whiz.hs src/Ho/Binary.hs src/Ho/Collected.hs src/Ho/Library.hs src/Info/Binary.hs src/Info/Info.hs src/Info/Properties.hs \
	src/Info/Types.hs src/Interactive.hs src/Main.hs src/Name/Binary.hs src/Name/Id.hs src/Name/Name.hs src/Name/Names.hs \
	src/Name/Prim.hs src/PackedString.hs src/PrimitiveOperators.hs \
	src/RawFiles.hs src/RtsFiles.hs src/Stats.hs src/Support/CanType.hs src/Support/CFF.hs src/Support/Compat.hs \
	src/Support/FreeVars.hs src/Support/MapBinaryInstance.hs src/Support/MD5.hs src/Support/Tickle.hs src/Support/Transform.hs \
	src/Support/Tuple.hs src/Support/Unparse.hs src/Util/BitSet.hs src/Util/ContextMonad.hs src/Util/FilterInput.hs \
	src/Util/Gen.hs src/Util/Graph.hs src/Util/Graphviz.hs src/Util/HasSize.hs src/Util/Histogram.hs src/Util/Inst.hs \
	src/Util/IntBag.hs src/Util/Interact.hs src/Util/NameMonad.hs src/Util/Once.hs src/Util/Perhaps.hs src/Util/ReaderWriter.hs \
	src/Util/Relation.hs src/Util/RWS.hs src/Util/SameShape.hs src/Util/Seq.hs src/Util/SetLike.hs src/Util/UnionFind.hs \
	src/Util/UnionSolve.hs src/Util/UniqueMonad.hs src/Util/Util.hs src/Util/VarName.hs src/Version/Config.hs src/Version/Version.hs \
	src/Support/IniParse.hs src/E/Lint.hs src/Util/Progress.hs src/Grin/StorageAnalysis.hs src/Util/YAML.hs src/Grin/Main.hs \
        src/E/Main.hs


GHCDEBUGOPTS= -W -fno-warn-unused-matches  -fwarn-type-defaults
GHCPROFOPTS= -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCSELFTESTOPTS= -osuf test.o -hisuf test.hi -DEXPORT_SELFTEST=,_selftest
GHCNONSELFTESTOPTS= -DEXPORT_SELFTEST=
GHCINC=  -i -i$(srcdir)/drift_processed  \
	 -i$(srcdir)/src  -i$(builddir)/src -odir $(builddir)/src -hidir $(builddir)/src

PACKAGES= -package fgl -package regex-compat -package random -package array -package directory \
          -package bytestring -package binary -package pretty -package mtl -package containers   \
	  -package unix  -package haskell98 -package utf8-string  -package zlib


GHCLANG= -XViewPatterns -XUndecidableInstances -XOverlappingInstances -fglasgow-exts -XRecordWildCards -XRecursiveDo -XTupleSections
GHCOPTS= $(HSOPTS) -XBangPatterns -O @GHCFLAGS@  $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) $(GHCLANG)

JHC_LIBS =  jhc-1.0.hl base-1.0.hl haskell98-1.0.hl applicative-1.0.hl containers-0.2.0.hl


EXTRA_DIST = src/data src/rts utils docs src/FrontEnd/HsParser.y $(BUILT_SOURCES) \
             lib/jhc lib/base lib/haskell98 lib/haskell98.cabal $(JHC_LIBS)    \
	     src/StringTable/StringTable_cbits.c src/StringTable/StringTable_cbits.h src/cbits/md5sum.c jhc.1 \
	     src/StringTable/Atom.hsc jhc.spec lib/applicative/applicative.cabal lib/containers.cabal \
	     manual.html lib/containers lib/applicative examples/HelloWorld.hs examples/Calendar.hs \
	     src/FlagOpts.flags src/FlagDump.flags

CC = $(HC)

# we recreate the manual and man page here just to ensure they are up to date
dist-hook: man manual
	cp $(srcdir)/manual.html $(distdir)/manual.html
	cp $(srcdir)/jhc.1 $(distdir)/jhc.1
	find $(distdir) \( -name _darcs -o -name \*.ho \)  -print | xargs rm -rf --

darcs-fetch:
	test -d src/Doc || darcs get --partial http://repetae.net/repos/Doc --repodir=src/Doc
	test -d lib/haskell98 || darcs get --partial http://darcs.haskell.org/packages/haskell98 --repodir=lib/haskell98
	test -d lib/containers || darcs get --partial http://darcs.haskell.org/packages/containers --repodir=lib/containers


all-local: libs

install-data-hook: $(JHC_LIBS)
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@
	for lib in $(JHC_LIBS); do  \
		test -f "$$lib" && $(INSTALL_DATA) $$lib $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
		test -f "$(srcdir)/$$lib" && $(INSTALL_DATA) "$(srcdir)/$$lib" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
	done
	$(INSTALL_DATA) "$(srcdir)/src/data/HsFFI.h" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	$(INSTALL_DATA) "$(srcdir)/src/data/targets.ini" $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@

uninstall-hook: $(JHC_LIBS)
	rm -f -- "$(DESTDIR)$(bindir)/jhci$(EXTEXT)"
	for lib in $(JHC_LIBS); do  \
		rm -f -- "$(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/$$lib" ; \
	done
	rm -f -- "$(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include/HsFFI.h"
	rm -f -- "$(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@/targets.ini"
	-rmdir $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	-rmdir $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	-rmdir $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = src/data/ViaGhc.hs ChangeLog \
	   src/data/shortchange.txt src/data/prelude.m4 src/data/targets.ini

RTSFILES = src/data/HsFFI.h src/rts/wsize.h src/rts/jhc_rts_header.h src/rts/jhc_jgc.h \
	   src/rts/jhc_rts_alloc.c src/rts/jhc_rts.c src/rts/profile.c src/rts/jhc_rts2.c src/rts/bitarray.h \
	   src/rts/slub.c src/rts/jhc_jgc.c  src/rts/lib_cbits.c

DRIFTFILES = drift_processed/C/FFI.hs drift_processed/C/FromGrin2.hs drift_processed/Cmm/Op.hs drift_processed/C/Prims.hs drift_processed/DataConstructors.hs \
   drift_processed/DerivingDrift/StandardRules.hs drift_processed/E/CPR.hs drift_processed/E/Demand.hs drift_processed/E/LambdaLift.hs \
   drift_processed/E/SSimplify.hs drift_processed/E/ToHs.hs drift_processed/E/TypeCheck.hs drift_processed/E/Type.hs drift_processed/FrontEnd/Class.hs \
   drift_processed/FrontEnd/Exports.hs drift_processed/FrontEnd/HsSyn.hs drift_processed/FrontEnd/KindInfer.hs \
   drift_processed/FrontEnd/Representation.hs drift_processed/FrontEnd/SrcLoc.hs drift_processed/FrontEnd/Tc/Kind.hs \
   drift_processed/FrontEnd/Tc/Monad.hs drift_processed/Grin/SSimplify.hs drift_processed/Name/VConsts.hs drift_processed/Options.hs \
   drift_processed/DataConstructors.hs-boot drift_processed/Ho/Type.hs  drift_processed/Ho/Build.hs

drift_processed/%: src/%
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	DrIFT $< -o $@


CFILES =  src/StringTable/StringTable_cbits.o  src/cbits/md5sum.o

%.o : %.c
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	$(HC) $(MY_CFLAGS) $< -c -o $@

jhc: src/Main.hs  $(HSFILES) $(BUILT_SOURCES) src/StringTable/Atom.hs $(CFILES)
	$(HC) $(GHCOPTS) --make $< $(CFILES) -o $@

jhcp: src/Main.hs $(BUILT_SOURCES) $(HSFILES) src/StringTable/Atom.hs $(CFILES)
	$(HC) $(GHCOPTS) $(GHCPROFOPTS) --make $< $(CFILES) -o $@

dist_man_MANS = jhc.1

install-exec-hook:
	cd $(DESTDIR)$(bindir) && $(LN_S) -f jhc$(EXEEXT) jhci$(EXEEXT)

HSBOOT= src/DataConstructors.hs-boot src/FrontEnd/Tc/Class.hs-boot src/Grin/Grin.hs-boot src/Grin/Show.hs-boot src/Info/Binary.hs-boot src/E/Show.hs-boot

# Various rules for generated Haskell files


%.hs: %.hsc
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	$(HSC2HS) $(MY_CINCLUDES) -o $@ $<

%.hs: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $<  > $@

%.mkd: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -f f -m -n $< $<  > $@

src/Info/Properties.hs: src/data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@


src/PrimitiveOperators.hs: utils/op_process.prl src/data/operators.txt src/data/primitives.txt src/data/PrimitiveOperators-in.hs
	perl $< > $@ || rm -f $@

src/Name/Prim.hs: utils/op_names.prl src/data/primitives.txt src/data/names.txt
	perl $< > $@ || rm -f $@

src/RawFiles.hs:  utils/op_raw.prl $(RAWFILES)
	perl $< $(basename $@)  $(RAWFILES) > $@

src/RtsFiles.hs:  utils/op_raw.prl $(RTSFILES)
	perl $< -c $(basename $@)  $(RTSFILES) > $@

src/FrontEnd/HsParser.hs: src/FrontEnd/HsParser.y
	happy -a -g -c $< -o $@

#	sed -e 's/^{-# OPTIONS[A-Z_]*/{-# OPTIONS_GHC -w /' -i $@


.SECONDARY: _darcs/hashed_inventory src/data/shortchange.txt

ChangeLog: _darcs/hashed_inventory
	darcs changes --from-tag . > $@

src/data/shortchange.txt: ChangeLog
	echo -n "`sed -ne 's/^  tagged \(.*\)/\1/p' $<`-`grep -c '^  \*' $<`"  > $@


BINDISTDIR=$(PACKAGE)-$(VERSION)-$(host)

bin-dist: jhc
	rm -rf -- $(BINDISTDIR)
	mkdir $(BINDISTDIR)
	strip -- jhc || true
	cp -- jhc $(BINDISTDIR)
	tar cvf $(BINDISTDIR).tar $(BINDISTDIR)
	gzip -f -- $(BINDISTDIR).tar
	rm -rf -- $(BINDISTDIR)


.INTERMEDIATE: deps.txt
.DELETE_ON_ERROR: deps.txt depend.make ChangeLog data/shortchange.txt

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(GHCOPTS) src/Main.hs

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/._]+.hs' $< | sed -e 's/^\.\///' | sort | uniq` > depend.make

built-sources: $(BUILT_SOURCES)

i: $(BUILT_SOURCES)
	$(HC) --interactive $(GHCOPTS) src/Main.hs $(CFILES)

.PHONY: ho-clean hl-clean built-sources i manual man

ho-clean:
	rm -f -- `find -name \*.ho -print`
hl-clean:
	rm -f -- `find -name \*.hl -print`

clean-local:
	find . \( \! -path \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) \) -print | xargs rm -f --

MAINTAINERCLEANFILES= $(BUILT_SOURCES) $(DRIFTFILES)

CLEANFILES= $(JHC_LIBS)

LIB_OPTIONS=$(RTSOPTS) $(JHC_TEST) --noauto -L- -L.


jhc-1.0.hl: lib/jhc/jhc.cabal
	./jhc $(LIB_OPTIONS) -ilib/jhc --build-hl $< -o $@

base-1.0.hl: lib/base/base.cabal jhc-1.0.hl
	./jhc $(LIB_OPTIONS) -ilib/base -pjhc --build-hl $< -o $@

applicative-1.0.hl: lib/applicative/applicative.cabal base-1.0.hl
	./jhc $(LIB_OPTIONS) -ilib/applicative -pjhc -pbase --build-hl $< -o $@

containers-0.2.0.hl: lib/containers.cabal base-1.0.hl applicative-1.0.hl
	./jhc $(LIB_OPTIONS) -Isrc/data -Ilib/containers/include -ilib/containers -pjhc -pbase -papplicative -fcpp --build-hl $<  -o $@

haskell98-1.0.hl: lib/haskell98.cabal base-1.0.hl
	./jhc $(LIB_OPTIONS) -ilib/haskell98 -pjhc -pbase --build-hl $< -o $@

%.pdf: %.ps
	epstopdf $< --outfile $@

%.ps : %.dot
	dot $< -Tps -o$@

%.pdf : %.dot
	dot $< -Tpdf -o$@

%.html: %.mkd
	pandoc $< -s -f markdown -t html -o $@

%.shtml: %.mkd
	cat $(srcdir)/docs/template/my_header.shtml > $@
	pandoc $<  -f markdown -t html >> $@
	cat $(srcdir)/docs/template/my_footer.shtml >> $@

publish: docs/building.shtml docs/big-picture.pdf docs/development.shtml docs/index.shtml docs/jhc.shtml manual.html docs/manual.css
	cp -- $^ /home/john/public_html/computer/jhc

manual: utils/stitch.prl src/FlagDump.mkd src/FlagOpts.mkd options.mkd docs/*.mkd
	find . \( ! -path */jhc-*/* ! -path '*/examples/*'  ! -path '*/_darcs/*' ! -path '*/drift_processed/*'  ! -path '*/regress/*'  \( -name '*.hs' -o -name '*.hsc' -o -name '*.mkd' -o -path '*/src/rts/*.c' \) \) -print | xargs perl utils/stitch.prl > manual.mkd
	pandoc manual.mkd --toc -s -f markdown -t html -s -c manual.css -o $@.html

man: utils/stitch.prl src/FlagDump.mkd src/FlagOpts.mkd options.mkd docs/man_header.mkd docs/*.mkd
	find . \( ! -path */jhc-*/ ! -path '*/examples/*'  ! -path '*/_darcs/*' ! -path '*/drift_processed/*'  ! -path '*/regress/*'  \( -name '*.hs' -o -name '*.hsc' -o -name '*.mkd' -o -path '*/src/rts/*.c' \) \) -print | xargs perl utils/stitch.prl -h docs/man_header.mkd -c Using,Options > jhc_man.mkd
	pandoc jhc_man.mkd -s -f markdown -t man -s  -o jhc.1

options.mkd: jhc
	echo "{-@Options 1" > $@
	./jhc --help | sed -e 's/^/    /'  >> $@

jhc.1 :
	$(MAKE) man

manual.html :
	$(MAKE) manual

.PHONY: libs
libs: $(JHC_LIBS)

%: examples/%.hs jhc
	./jhc  -L- -L. $(JHC_TEST) $< -o $@ 2>&1 | tee $@.log

