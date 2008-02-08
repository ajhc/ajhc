
ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = Main.hs  $(HSFILES) $(BUILT_SOURCES) $(HSBOOT)

SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot

BUILT_SOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs \
	       FlagOpts.hs Name/Prim.hs Info/Properties.hs


HSFILES= Atom.hs C/Arch.hs C/FFI.hs C/FromGrin2.hs C/Generate.hs CharIO.hs C/OpEval.hs C/Op.hs \
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
	 Info/Properties.hs Info/Types.hs Interactive.hs Main.hs MapBinaryInstance.hs Name/Binary.hs Name/Id.hs \
	 Name/Name.hs Name/Names.hs Name/Prim.hs Name/VConsts.hs Number.hs Options.hs PackedString.hs \
	 PrimitiveOperators.hs RawFiles.hs Stats.hs Support/CanType.hs Support/CFF.hs Support/FreeVars.hs \
	 Support/Tickle.hs Support/Transform.hs Support/Tuple.hs Support/Unparse.hs UTF8.hs Util/BitSet.hs \
	 Util/ContextMonad.hs Util/FilterInput.hs Util/Gen.hs Util/Graph.hs Util/Graphviz.hs Util/HasSize.hs \
	 Util/Histogram.hs Util/Inst.hs Util/IntBag.hs Util/Interact.hs \
         Util/NameMonad.hs Util/Once.hs Util/Perhaps.hs Util/ReaderWriter.hs \
         Util/Relation.hs Util/RWS.hs Util/SameShape.hs Util/Seq.hs Util/SetLike.hs \
         Util/SHA1.hs Util/UnionFind.hs Util/UnionSolve.hs Util/UniqueMonad.hs \
         Util/VarName.hs Version/Version.hs Version/Config.hs

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

EXTRA_DIST = data utils arch docs FrontEnd/HsParser.y $(BUILT_SOURCES)

install-data-hook: $(JHC_LIBS)
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	$(INSTALL_DATA) $(JHC_LIBS) $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = data/HsFFI.h data/jhc_rts.c data/jhc_rts_header.h data/wsize.h data/jhc_rts_alloc.c data/jhc_rts2.c data/ViaGhc.hs ChangeLog shortchange.txt


jhc: Main.hs  $(HSFILES) $(BUILT_SOURCES)
	$(HC) $(GHCOPTS) --make $< -o $@

jhcp: Main.hs $(BUILT_SOURCES) $(HSFILES)
	$(HC) $(GHCOPTS) $(GHCPROFOPTS) --make $< -o $@

jhc.1: Options.hs
	$(MAKE) $(AM_MAKEFLAGS) jhc$(EXEEXT)
	help2man -N --output=jhc.1 ./jhc$(EXEEXT)

dist_man_MANS = jhc.1

install-exec-hook:
	cd $(DESTDIR)$(bindir) && $(LN_S) -f jhc$(EXEEXT) jhci$(EXEEXT)

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


.SECONDARY: _darcs/inventory shortchange.txt

ChangeLog: _darcs/inventory
	darcs changes --from-tag . > $@

shortchange.txt: ChangeLog
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

rpm: jhc.spec dist
	cp jhc-$(VERSION).tar.gz $(HOME)/var/rpm/SOURCES/jhc-$(SHORTVERSION).tar.gz
	rpmbuild -ba jhc.spec


.INTERMEDIATE: deps.txt
.DELETE_ON_ERROR: deps.txt depend.make ChangeLog shortchange.txt

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(HC_OPTS) Main.hs

update-datestamp: ChangeLog
	sh utils/update_datestamp.sh $< configure.ac

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/.]+.hs' $< | sed -e 's/^\.\///' | sort` > depend.make

i:
	ghci $(GHCOPTS) Main.hs

.PHONY: ho-clean hl-clean i update-datestamp

ho-clean:
	rm -f -- `find -name \*.ho`
hl-clean:
	rm -f -- `find -name \*.hl`

clean-local:
	find . \! -wholename \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) | xargs rm -f --

MAINTAINERCLEANFILES= $(BUILT_SOURCES)

base-1.0.hl: lib/base/base.cabal
	./jhc -v -funboxed-tuples $(RTSOPTS) $(JHC_TEST)  -ilib/base --noauto --build-hl lib/base/base.cabal -o $@

haskell98-1.0.hl: jhc lib/haskell98/haskell98.cabal base-1.0.hl
	./jhc -v $(RTSOPTS) $(JHC_TEST) -ilib/haskell98 --noauto -L- -L. -p base --build-hl lib/haskell98.cabal -o $@


libs: $(JHC_LIBS)

helloworld: test/HelloWorld.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L. $(JHC_TEST) test/HelloWorld.hs -o $@ 2>&1 | tee $@.log
calendar: test/Calendar.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L. $(JHC_TEST) test/Calendar.hs -o $@ 2>&1 | tee $@.log
primes: test/Primes.hs jhc
	-[ -e $@.log ] && mv -f $@.log $@.log.bak
	./jhc -v -L. $(JHC_TEST) test/Primes.hs -o $@ 2>&1 | tee $@.log
