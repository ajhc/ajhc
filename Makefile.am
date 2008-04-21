
ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc


jhc_SOURCES = Main.hs  $(HSFILES) $(BUILT_SOURCES) $(HSBOOT) $(DRIFTFILES)

SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot

BUILT_SOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs \
	       FlagOpts.hs Name/Prim.hs Info/Properties.hs $(DRIFTFILES)

MY_CINCLUDES= -I$(srcdir)/StringTable -I$(srcdir)/cbits -I$(builddir)/cbits
MY_CFLAGS= $(MY_CINCLUDES) -optc-std=c99 -optc-O2 -optc-Wall

HSFILES= C/Arch.hs C/FFI.hs C/FromGrin2.hs C/Generate.hs CharIO.hs Cmm/OpEval.hs Cmm/Op.hs \
         C/Prims.hs DataConstructors.hs DataConstructors.hs DerivingDrift/DataP.hs DerivingDrift/Drift.hs \
	 DerivingDrift/RuleUtils.hs DerivingDrift/StandardRules.hs Doc/Attr.hs Doc/Chars.hs Doc/DocLike.hs \
	 Doc/PPrint.hs Doc/Pretty.hs E/Annotate.hs E/Binary.hs E/CPR.hs E/Demand.hs E/Diff.hs E/E.hs \
	 E/Eta.hs E/Eval.hs E/FreeVars.hs E/FromHs.hs E/Inline.hs E/LambdaLift.hs E/LetFloat.hs E/PrimOpt.hs \
	 E/Program.hs E/Rules.hs E/Show.hs E/SSimplify.hs E/Subst.hs E/Traverse.hs E/TypeAnalysis.hs \
	 E/TypeCheck.hs E/Type.hs E/Values.hs E/WorkerWrapper.hs FindFixpoint.hs Fixer/Fixer.hs Fixer/Supply.hs \
	 Fixer/VMap.hs FlagDump.hs FlagOpts.hs FrontEnd/Class.hs FrontEnd/DataConsAssump.hs FrontEnd/DeclsDepends.hs \
	 FrontEnd/DependAnalysis.hs FrontEnd/Desugar.hs FrontEnd/Diagnostic.hs FrontEnd/Exports.hs \
	 FrontEnd/FrontEnd.hs FrontEnd/HsErrors.hs FrontEnd/HsParser.hs FrontEnd/HsPretty.hs FrontEnd/HsSyn.hs \
	 FrontEnd/Infix.hs FrontEnd/KindInfer.hs FrontEnd/Lexer.hs FrontEnd/ParseMonad.hs FrontEnd/ParseUtils.hs \
	 FrontEnd/Rename.hs FrontEnd/Representation.hs FrontEnd/SrcLoc.hs FrontEnd/Syn/Options.hs \
	 FrontEnd/Syn/Traverse.hs FrontEnd/Tc/Class.hs FrontEnd/Tc/Class.hs FrontEnd/Tc/Kind.hs \
	 FrontEnd/Tc/Main.hs FrontEnd/Tc/Module.hs FrontEnd/Tc/Monad.hs FrontEnd/Tc/Type.hs \
	 FrontEnd/Tc/Unify.hs FrontEnd/TypeSigs.hs FrontEnd/TypeSynonyms.hs FrontEnd/TypeSyns.hs \
	 FrontEnd/Unlit.hs FrontEnd/Utils.hs FrontEnd/Warning.hs GenUtil.hs Grin/DeadCode.hs Grin/Devolve.hs \
	 Grin/EvalInline.hs Grin/FromE.hs Grin/Grin.hs Grin/HashConst.hs Grin/Lint.hs Grin/NodeAnalyze.hs \
	 Grin/Noodle.hs Grin/Optimize.hs Grin/Show.hs Grin/Simplify.hs Grin/SSimplify.hs Grin/Val.hs Grin/Whiz.hs \
	 Ho/Binary.hs Ho/Build.hs Ho/Library.hs Ho/Type.hs Info/Binary.hs Info/Binary.hs Info/Info.hs \
	 Info/Properties.hs Info/Types.hs Interactive.hs Main.hs Support/MapBinaryInstance.hs Name/Binary.hs Name/Id.hs \
	 Name/Name.hs Name/Names.hs Name/Prim.hs Name/VConsts.hs Cmm/Number.hs Options.hs PackedString.hs \
	 PrimitiveOperators.hs RawFiles.hs Stats.hs Support/CanType.hs Support/CFF.hs Support/FreeVars.hs \
	 Support/Tickle.hs Support/Transform.hs Support/Tuple.hs Support/Unparse.hs UTF8.hs Util/BitSet.hs \
	 Util/ContextMonad.hs Util/FilterInput.hs Util/Gen.hs Util/Graph.hs Util/Graphviz.hs Util/HasSize.hs \
	 Util/Histogram.hs Util/Inst.hs Util/IntBag.hs Util/Interact.hs \
         Util/NameMonad.hs Util/Once.hs Util/Perhaps.hs Util/ReaderWriter.hs \
         Util/Relation.hs Util/RWS.hs Util/SameShape.hs Util/Seq.hs Util/SetLike.hs \
         Util/SHA1.hs Util/UnionFind.hs Util/UnionSolve.hs Util/UniqueMonad.hs \
         Util/VarName.hs Version/Version.hs Version/Config.hs \
	 StringTable/Atom.hsc Util/Util.hs Support/MD5.hs Ho/Collected.hs

GHCDEBUGOPTS= -W -fno-warn-unused-matches  -fwarn-type-defaults
GHCPROFOPTS=   -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCINC=  -i -i$(srcdir)/drift_processed  \
	 -i$(srcdir)/.  -i$(builddir) -odir $(builddir) -hidir $(builddir)
PACKAGES= -package mtl  -package unix -package QuickCheck -ignore-package lang
GHCLANG=    -fallow-undecidable-instances  -fglasgow-exts -fallow-overlapping-instances
GHCOPTS=  $(HSOPTS) -fbang-patterns -O -ignore-package lang  $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) $(GHCLANG)

JHC_LIBS =  base-1.0.hl haskell98-1.0.hl

EXTRA_DIST = data utils arch docs FrontEnd/HsParser.y $(BUILT_SOURCES) lib/base lib/haskell98 lib/haskell98.cabal $(JHC_LIBS) StringTable/StringTable_cbits.c StringTable/StringTable_cbits.h cbits/md5sum.c

CC = $(HC)

dist-hook:
	find $(distdir)  -name _darcs -o -name \*.ho | xargs rm -rf --

install-data-hook: $(JHC_LIBS)
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	for lib in $(JHC_LIBS); do  \
		test -f "$$lib" && $(INSTALL_DATA) $$lib $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
		test -f "$(srcdir)/$$lib" && $(INSTALL_DATA) "$(srcdir)/$$lib" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
	done

uninstall-hook: $(JHC_LIBS)
	rm -f -- "$(DESTDIR)$(bindir)/jhci$(EXTEXT)"
	for lib in $(JHC_LIBS); do  \
		rm -f -- "$(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/$$lib" ; \
	done

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = data/HsFFI.h data/rts/jhc_rts.c data/rts/jhc_rts_header.h data/wsize.h data/rts/jhc_rts_alloc.c data/rts/jhc_rts2.c data/ViaGhc.hs ChangeLog data/shortchange.txt data/prelude.m4

DRIFTFILES = drift_processed/C/FFI.hs drift_processed/C/FromGrin2.hs drift_processed/Cmm/Op.hs drift_processed/C/Prims.hs drift_processed/DataConstructors.hs \
   drift_processed/DerivingDrift/StandardRules.hs drift_processed/E/CPR.hs drift_processed/E/Demand.hs drift_processed/E/LambdaLift.hs \
   drift_processed/E/SSimplify.hs drift_processed/E/ToHs.hs drift_processed/E/TypeCheck.hs drift_processed/E/Type.hs drift_processed/FrontEnd/Class.hs \
   drift_processed/FrontEnd/Exports.hs drift_processed/FrontEnd/HsSyn.hs drift_processed/FrontEnd/KindInfer.hs \
   drift_processed/FrontEnd/Representation.hs drift_processed/FrontEnd/SrcLoc.hs drift_processed/FrontEnd/Tc/Kind.hs \
   drift_processed/FrontEnd/Tc/Monad.hs drift_processed/Grin/SSimplify.hs drift_processed/Name/VConsts.hs drift_processed/Options.hs \
   drift_processed/DataConstructors.hs-boot drift_processed/Ho/Type.hs  drift_processed/Ho/Build.hs

drift_processed/%: %
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	DrIFT $< -o $@


CFILES =  StringTable/StringTable_cbits.o  cbits/md5sum.o

%.o : %.c
	$(HC) $(MY_CFLAGS) $< -c -o $@

jhc: Main.hs  $(HSFILES) $(BUILT_SOURCES) StringTable/Atom.hs $(CFILES)
	$(HC) $(GHCOPTS) --make $< $(CFILES) -o $@

jhcp: Main.hs $(BUILT_SOURCES) $(HSFILES) StringTable/Atom.hs $(CFILES)
	$(HC) $(GHCOPTS) $(GHCPROFOPTS) --make $< $(CFILES) -o $@

jhc.1: Options.hs
	$(MAKE) $(AM_MAKEFLAGS) jhc$(EXEEXT)
	help2man -N --output=jhc.1 ./jhc$(EXEEXT)

dist_man_MANS = jhc.1

install-exec-hook:
	cd $(DESTDIR)$(bindir) && $(LN_S) -f jhc$(EXEEXT) jhci$(EXEEXT)

HSBOOT= DataConstructors.hs-boot FrontEnd/Tc/Class.hs-boot Grin/Grin.hs-boot Grin/Show.hs-boot Info/Binary.hs-boot

# Various rules for generated Haskell files


%.hs: %.hsc
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	$(HSC2HS) $(MY_CINCLUDES) -o $@ $<

%.hs: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $<  > $@

%.mkd: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -f f -m -n $< $<  > $@

Info/Properties.hs: data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@


PrimitiveOperators.hs: utils/op_process.prl data/operators.txt data/primitives.txt data/PrimitiveOperators-in.hs
	perl $< > $@ || rm -f $@

Name/Prim.hs: utils/op_names.prl data/primitives.txt data/names.txt
	perl $< > $@ || rm -f $@

RawFiles.hs:  utils/op_raw.prl $(RAWFILES)
	perl $< $(basename $@)  $(RAWFILES) > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.y
	happy -a -g -c $< -o $@
	sed -e 's/^{-# OPTIONS/{-# OPTIONS_GHC -w/' -i $@


.SECONDARY: _darcs/inventory data/shortchange.txt

ChangeLog: _darcs/inventory
	darcs changes --from-tag . > $@

data/shortchange.txt: ChangeLog
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

rpmbuild: jhc.spec dist
	cp jhc-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/jhc-$(VERSION).tar.gz
	rpmbuild -ba $<

rpm: rpmbuild
	mkdir -p drop
	mv jhc-$(VERSION).tar.gz drop/jhc-$(VERSION).tar.gz
	mv ~/var/rpm/RPMS/*/jhc-$(VERSION)-$(RPMRELEASE).*.rpm drop
	mv ~/var/rpm/SRPMS/jhc-$(VERSION)-$(RPMRELEASE).src.rpm drop

publish:

.INTERMEDIATE: deps.txt
.DELETE_ON_ERROR: deps.txt depend.make ChangeLog data/shortchange.txt

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(GHCOPTS) Main.hs

update-datestamp: ChangeLog
	sh utils/update_datestamp.sh $< configure.ac

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/.]+.hs' $< | sed -e 's/^\.\///' | sort` > depend.make

built-sources: $(BUILT_SOURCES)

i: $(BUILT_SOURCES)
	$(HC) --interactive $(GHCOPTS) Main.hs $(CFILES)

.PHONY: ho-clean hl-clean built-sources i update-datestamp manual

ho-clean:
	rm -f -- `find -name \*.ho`
hl-clean:
	rm -f -- `find -name \*.hl`

clean-local:
	find . \! -wholename \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) | xargs rm -f --
	rm -rf -- drift_processed

MAINTAINERCLEANFILES= $(BUILT_SOURCES)

base-1.0.hl: lib/base/base.cabal
	mkdir -p tmp/libho
	./jhc -v -funboxed-tuples $(RTSOPTS) $(JHC_TEST)  --ho-dir tmp/libho -ilib/base --noauto --build-hl lib/base/base.cabal -o $@

haskell98-1.0.hl: lib/haskell98.cabal base-1.0.hl
	mkdir -p tmp/libho
	./jhc -v $(RTSOPTS) $(JHC_TEST) --ho-dir tmp/libho -ilib/haskell98 --noauto -L- -L. -p base --build-hl lib/haskell98.cabal -o $@

%.pdf: %.ps
	epstopdf $< --outfile $@

%.ps : %.dot
	dot $< -Tps -o$@

%.pdf : %.dot
	dot $< -Tpdf -o$@

%.html: %.mkd
	pandoc $< -s -f markdown -t html -o $@

%.shtml: %.mkd
	cat $(srcdir)/docs/my_header.shtml > $@
	pandoc $<  -f markdown -t html >> $@
	cat $(srcdir)/docs/my_footer.shtml >> $@

publish: docs/building.shtml docs/big-picture.pdf docs/development.shtml docs/index.shtml docs/jhc.shtml manual.html
	cp -- $^ /home/john/public_html/computer/jhc

manual: utils/stitch.prl FlagDump.mkd FlagOpts.mkd options.mkd
	find . ! -wholename '*/test/*'  ! -wholename '*/_darcs/*' ! -wholename '*/drift_processed/*'  ! -wholename '*/regress/*'  \( -name '*.hs' -o -name '*.hsc' -o -name '*.mkd' -o -wholename '*/data/rts/*.c' \) | xargs perl utils/stitch.prl > manual.mkd

options.mkd: jhc
	echo "{-@Options 1" > $@
	./jhc --help | sed -e 's/^/    /'  >> $@

manual.html : manual
	pandoc manual.mkd --toc -s -f markdown -t html -s -c wiki.css -o $@

libs: $(JHC_LIBS)

helloworld: test/HelloWorld.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L- -L. $(JHC_TEST) test/HelloWorld.hs -o $@ 2>&1 | tee $@.log
calendar: test/Calendar.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L- -L. $(JHC_TEST) test/Calendar.hs -o $@ 2>&1 | tee $@.log
primes: test/Primes.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L- -L. $(JHC_TEST) test/Primes.hs -o $@ 2>&1 | tee $@.log
