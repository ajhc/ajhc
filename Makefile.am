
ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = Main.hs  $(HSFILES) $(BUILT_SOURCES) $(HSBOOT)

SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot

BUILT_SOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs \
	       FlagOpts.hs Version/Raw.hs Version/Ctx.hs Name/Prim.hs Info/Properties.hs


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
         Util/VarName.hs Version/Ctx.hs Version.hs Version/Raw.hs

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

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = data/HsFFI.h data/jhc_rts.c data/jhc_rts_header.h data/wsize.h data/jhc_rts_alloc.c data/jhc_rts2.c data/ViaGhc.hs


jhc: Main.hs  $(HSFILES) $(BUILT_SOURCES)
	$(HC) $(GHCOPTS) --make $< -o $@

jhcp: Main.hs $(BUILT_SOURCES) $(HSFILES)
	$(HC) $(GHCOPTS) $(GHCPROFOPTS) --make $< -o $@

install-exec-hook:
	cd $(DESTDIR)$(bindir) && $(LN_S) jhc$(EXEEXT) jhci$(EXEEXT)

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


.INTERMEDIATE: deps.txt changes.txt
.DELETE_ON_ERROR: deps.txt depend.make

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(HC_OPTS) Main.hs


depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/.]+.hs' $< | sed -e 's/^\.\///' | sort` > depend.make

i:
	ghci $(GHCOPTS) Main.hs

PHONY= ho-clean hl-clean i

ho-clean:
	rm -f -- `find -name \*.ho`
hl-clean:
	rm -f -- `find -name \*.hl`

clean-local:
	find . \! -wholename \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) | xargs rm -f --

MAINTAINERCLEANFILES= $(BUILT_SOURCES)

base-1.0.hl: jhc lib/base/base.cabal
	-[ -e base.log ] && mv -f base.log base.log.bak
	set -o pipefail; ./jhc -v -funboxed-tuples $(RTSOPTS) $(JHC_TEST)  -ilib/base --noauto --build-hl lib/base/base.cabal -o $@ 2>&1 | tee base.log

base-1.0.prof.hl: jhc lib/base/base.cabal
	-[ -e base.prof.log ] && mv -f base.prof.log base.prof.log.bak
	./jhcp -v $(RTSOPTS) $(JHC_TEST) -ilib/base --noauto --build-hl lib/base/base.cabal -o base-1.0.prof.hl +RTS $(PROF_OPTS)  2>&1 | tee base.log
