ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = src/Main.hs $(HSFILES) $(BUILT_SOURCES) $(HSBOOT) $(DRIFTFILES)

BUILT_SOURCES= $(DRIFTFILES) src/FlagDump.hs src/FlagOpts.hs src/FrontEnd/Lex/Lexer.hs \
    src/FrontEnd/Lex/Parser.hs src/Info/Properties.hs src/Name/Prim.hs src/RawFiles.hs

MY_CINCLUDES= -I$(srcdir)/src/data -I$(srcdir)/src/StringTable -I$(srcdir)/src/cbits -I$(builddir)/src/cbits
MY_CFLAGS= $(MY_CINCLUDES) -optc-std=c99 -optc-O2 -optc-Wall
CC = $(HC)

HSFILES= \
    src/C/Generate.hs src/Cmm/Number.hs src/Cmm/OpEval.hs src/DerivingDrift/DataP.hs \
    src/DerivingDrift/Drift.hs src/DerivingDrift/RuleUtils.hs src/DerivingDrift/StandardRules.hs \
    src/Doc/Attr.hs src/Doc/Chars.hs src/Doc/DocLike.hs src/Doc/PPrint.hs src/Doc/Pretty.hs \
    src/E/Annotate.hs src/E/Binary.hs src/E/Diff.hs src/E/E.hs src/E/Eta.hs src/E/Eval.hs \
    src/E/FreeVars.hs src/E/FromHs.hs src/E/Inline.hs src/E/LambdaLift.hs src/E/LetFloat.hs \
    src/E/Lint.hs src/E/Main.hs src/E/PrimDecode.hs src/E/PrimOpt.hs src/E/Program.hs \
    src/E/Rules.hs src/E/Show.hs src/E/Subst.hs src/E/Traverse.hs src/E/TypeAnalysis.hs \
    src/E/TypeCheck.hs src/E/Values.hs src/E/WorkerWrapper.hs src/FindFixpoint.hs \
    src/Fixer/Fixer.hs src/Fixer/Supply.hs src/Fixer/VMap.hs src/FlagDump.hs src/FlagOpts.hs \
    src/FrontEnd/DataConsAssump.hs src/FrontEnd/DependAnalysis.hs \
    src/FrontEnd/Desugar.hs src/FrontEnd/Diagnostic.hs src/FrontEnd/Exports.hs \
    src/FrontEnd/FrontEnd.hs src/FrontEnd/HsErrors.hs src/FrontEnd/HsPretty.hs \
    src/FrontEnd/Infix.hs src/FrontEnd/Lex/Fixity.hs src/FrontEnd/Lex/Layout.hs \
    src/FrontEnd/Lex/Lexer.hs src/FrontEnd/Lex/Parse.hs src/FrontEnd/Lex/ParseMonad.hs \
    src/FrontEnd/Lex/Parser.hs src/FrontEnd/Lex/Post.hs src/FrontEnd/Rename.hs \
    src/FrontEnd/Syn/Options.hs src/FrontEnd/Syn/Traverse.hs src/FrontEnd/Tc/Class.hs \
    src/FrontEnd/Tc/Main.hs src/FrontEnd/Tc/Module.hs src/FrontEnd/Tc/Type.hs \
    src/FrontEnd/Tc/Unify.hs src/FrontEnd/TypeSigs.hs src/FrontEnd/TypeSynonyms.hs \
    src/FrontEnd/Unlit.hs src/FrontEnd/Warning.hs src/GenUtil.hs src/Grin/DeadCode.hs \
    src/Grin/Devolve.hs src/Grin/EvalInline.hs src/Grin/FromE.hs src/Grin/Grin.hs \
    src/Grin/HashConst.hs src/Grin/Lint.hs src/Grin/Main.hs src/Grin/NodeAnalyze.hs \
    src/Grin/Noodle.hs src/Grin/Optimize.hs src/Grin/Show.hs src/Grin/StorageAnalysis.hs \
    src/Grin/Val.hs src/Grin/Whiz.hs src/Ho/Binary.hs src/Ho/Build.hs src/Ho/Collected.hs \
    src/Ho/Library.hs src/Ho/ReadSource.hs src/Info/Binary.hs src/Info/Info.hs \
    src/Info/Properties.hs src/Info/Types.hs src/Interactive.hs src/Main.hs src/Name/Binary.hs \
    src/Name/Id.hs src/Name/Internals.hs src/Name/Name.hs src/Name/Names.hs src/Name/Prim.hs \
    src/Name/VConsts.hs src/Options.hs src/Options/Map.hs src/Options/Type.hs src/PackedString.hs \
    src/RawFiles.hs src/Stats.hs src/Support/CFF.hs src/Support/CanType.hs src/Support/Compat.hs \
    src/Support/FreeVars.hs src/Support/IniParse.hs src/Support/MD5.hs \
    src/Support/MapBinaryInstance.hs src/Support/TempDir.hs src/Support/Tickle.hs \
    src/Support/Transform.hs src/Support/Tuple.hs src/Support/Unparse.hs src/Ty/Level.hs \
    src/Util/BitSet.hs src/Util/ContextMonad.hs src/Util/ExitCodes.hs src/Util/FilterInput.hs \
    src/Util/GMap.hs src/Util/Gen.hs src/Util/Graph.hs src/Util/Graphviz.hs src/Util/HasSize.hs \
    src/Util/Histogram.hs src/Util/Inst.hs src/Util/IntBag.hs src/Util/Interact.hs \
    src/Util/NameMonad.hs src/Util/Once.hs src/Util/Perhaps.hs src/Util/Progress.hs \
    src/Util/RWS.hs src/Util/ReaderWriter.hs src/Util/Relation.hs src/Util/SameShape.hs \
    src/Util/Seq.hs src/Util/SetLike.hs src/Util/Std.hs src/Util/UnionFind.hs \
    src/Util/UnionSolve.hs src/Util/UniqueMonad.hs src/Util/Util.hs src/Util/VarName.hs \
    src/Util/YAML.hs src/Version/Config.hs src/Version/Version.hs src/Util/DocLike.hs \
    src/Util/UnionFindST.hs src/FrontEnd/Syn/Q.hs src/Deriving/Ix.hs src/Deriving/Text.hs src/Deriving/Ord.hs \
    src/Deriving/Derive.hs src/Deriving/Type.hs

GHCDEBUGOPTS= -W -fno-warn-unused-matches  -fwarn-type-defaults
GHCPROFOPTS= -prof -fprof-auto -rtsopts -auto-all -odir @builddir@/obj/prof -hidir @builddir@/obj/prof
GHCNORMOPTS= -odir @builddir@/obj/norm -hidir @builddir@/obj/norm
GHCNONSELFTESTOPTS= -DEXPORT_SELFTEST=
GHCINC=  -i @GHCINC@ -i$(srcdir)/drift_processed -i$(srcdir)/src -i$(builddir)/src \
	 -I$(srcdir)/src -I$(builddir)/src

GHCLANG= \
    -XTypeFamilies -XViewPatterns -XUndecidableInstances -XOverlappingInstances -XRecordWildCards \
    -XRecursiveDo -XTupleSections -XParallelListComp -XGeneralizedNewtypeDeriving \
    -XScopedTypeVariables -XFlexibleInstances -XTypeSynonymInstances -XMultiParamTypeClasses \
    -XDeriveDataTypeable -XNoMonoLocalBinds -XNamedFieldPuns -XForeignFunctionInterface \
    -XUnliftedFFITypes -XImplicitParams -XUnboxedTuples -XStandaloneDeriving -XDeriveFunctor \
    -XDeriveFoldable -XDeriveTraversable -XDeriveGeneric -XFlexibleContexts \
    -XConstrainedClassMethods -XFunctionalDependencies -XMagicHash -XPolymorphicComponents \
    -XExistentialQuantification -XUnicodeSyntax -XPostfixOperators -XPatternGuards \
    -XLiberalTypeSynonyms -XRankNTypes -XTypeOperators -XEmptyDataDecls -XKindSignatures \
    -XBangPatterns

GHCOPTS= $(HSOPTS) -O @GHCFLAGS@ $(GHCDEBUGOPTS) $(GHCINC) $(GHCLANG)

LIB_OPTIONS=$(RTSOPTS) $(JHC_TEST)  -L- -L.
include library_deps.make

EXTRA_DIST = src/data rts utils docs $(BUILT_SOURCES) \
             lib/jhc lib/jhc-prim lib/haskell-extras lib/haskell98 lib/haskell2010 lib/flat-foreign $(JHC_LIBS) $(JHC_EXT_LIBS) \
	     src/StringTable/StringTable_cbits.c src/StringTable/StringTable_cbits.h src/cbits/md5sum.c jhc.1 \
	     src/StringTable/Atom.hsc jhc.spec src/cbits/lookup3.c \
	     docs/manual.html lib/applicative examples/HelloWorld.hs examples/Calendar.hs \
	     examples/Primes.hs src/FlagOpts.flags src/FlagDump.flags lib/ext compat LICENSE \
	     src/FrontEnd/Lex/Lexer.x src/FrontEnd/Lex/Parser.y.pp  src/FrontEnd/Lex/Parser.y

dist-hook: $(BUILT_SOURCES)
	if test -f _darcs; then \
		rm -f $(srcdir)/ChangeLog $(srcdir)/src/data/shortchange.txt; \
		$(MAKE) ChangeLog src/data/shortchange.txt src/RawFiles.hs;   \
		cp -f $(srcdir)/ChangeLog $(destdir)/ChangeLog; \
		cp -f $(srcdir)/src/data/shortchange.txt $(destdir)/src/data/shortchange.txt; \
		cp -f $(srcdir)/src/RawFiles.hs $(destdir)/src/RawFiles.hs; \
	fi

all-local: $(JHC_LIBS)

install-data-hook: $(JHC_LIBS) $(JHC_EXT_LIBS)
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@
	for lib in $(JHC_LIBS) $(JHC_EXT_LIBS); do \
		test -f "$$lib" && $(INSTALL_DATA) $$lib $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
		test -f "$(srcdir)/$$lib" && $(INSTALL_DATA) "$(srcdir)/$$lib" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
	done
	$(INSTALL_DATA) "$(srcdir)/rts/HsFFI.h" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	$(INSTALL_DATA) "$(srcdir)/src/data/targets.ini" $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@

uninstall-hook:
	rm -f -- "$(DESTDIR)$(bindir)/jhci$(EXTEXT)"
	for lib in $(JHC_LIBS) $(JHC_EXT_LIBS); do \
		rm -f -- "$(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/$$lib" ; \
	done
	rm -f -- "$(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include/HsFFI.h"
	rm -f -- "$(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@/targets.ini"
	-rmdir $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	-rmdir $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	-rmdir $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@

UTILS = utils/op_process.prl utils/opt_sets.prl utils/gen_props.prl utils/op_names.prl utils/op_raw.prl

RAWFILES = src/data/ViaGhc.hs src/data/prelude.m4 \
	   src/data/targets.ini rts/rts/constants.h rts/rts/stableptr.c rts/sys/queue.h rts/HsFFI.h \
           rts/sys/wsize.h rts/sys/bitarray.h ChangeLog src/data/shortchange.txt \
	   rts/rts/gc_jgc.c rts/rts/gc_jgc.h rts/rts/profile.c rts/rts/profile.h rts/rts/cdefs.h rts/rts/rts_support.c \
	   rts/rts/rts_support.h rts/rts/gc.h rts/rts/gc_none.c rts/rts/gc_none.h rts/rts/jhc_rts.c rts/rts/jhc_rts.h \
	   rts/lib/lib_cbits.c rts/jhc_rts_header.h rts/lib/lib_cbits.h rts/rts/gc_jgc_internal.h

DRIFTFILES = drift_processed/C/FFI.hs drift_processed/C/FromGrin2.hs \
    drift_processed/C/Prims.hs drift_processed/Cmm/Op.hs \
    drift_processed/DataConstructors.hs drift_processed/DataConstructors.hs-boot \
    drift_processed/E/CPR.hs drift_processed/E/Demand.hs \
    drift_processed/E/SSimplify.hs drift_processed/E/ToHs.hs \
    drift_processed/E/Type.hs drift_processed/FrontEnd/Class.hs \
    drift_processed/FrontEnd/HsSyn.hs drift_processed/FrontEnd/KindInfer.hs \
    drift_processed/FrontEnd/Representation.hs drift_processed/FrontEnd/SrcLoc.hs \
    drift_processed/FrontEnd/Tc/Kind.hs drift_processed/FrontEnd/Tc/Monad.hs \
    drift_processed/Grin/SSimplify.hs drift_processed/Ho/Type.hs \
    drift_processed/Options.hs drift_processed/Options/Type.hs

drift_processed/%: src/%
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	DrIFT $< -o $@

CFILES = src/StringTable/StringTable_cbits.o src/cbits/md5sum.o src/cbits/lookup3.o

%.o : %.c
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	$(HC) $(MY_CFLAGS) $< -c -o $@

jhc$(EXEEXT): src/Main.hs  $(HSFILES) $(BUILT_SOURCES) src/StringTable/Atom.hs $(CFILES)
	$(BUILD_STOW) $(HC) $(GHCOPTS) $(GHCNORMOPTS) --make $< $(CFILES) -o $@

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

%.hs: %.flags utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -n $< $< > $@

%.mkd: %.flags utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -f f -m -n $< $< > $@

src/Info/Properties.hs: src/data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@

src/Name/Prim.hs: src/data/names.txt  utils/op_names.prl
	perl utils/op_names.prl -i $< > $@ || rm -f $@

src/RawFiles.hs: $(RAWFILES)
	perl $(srcdir)/utils/op_raw.prl $(basename $@) $^ > $@

.SECONDARY: _darcs/hashed_inventory src/data/shortchange.txt

ChangeLog:
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

.INTERMEDIATE: deps.txt jhc_man.mkd options.mkd
.DELETE_ON_ERROR: deps.txt depend.make ChangeLog data/shortchange.txt manual.mkd jhc_man.mkd options.mkd

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(GHCOPTS) src/Main.hs

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/._]+.hs' $< | sed -e 's/^\.\///' | sort | uniq` > depend.make

built-sources: $(BUILT_SOURCES)

i: $(BUILT_SOURCES)
	$(HC) --interactive $(GHCOPTS) -odir $(builddir)/obj/i -hidir $(builddir)/obj/i -fobject-code src/Main.hs $(CFILES)

i2: $(BUILT_SOURCES)
	$(HC) --interactive $(GHCOPTS)  $(CFILES)

ltest: src/FrontEnd/Lex/Parse.hs $(BUILT_SOURCES) src/FrontEnd/Lex/Lexer.hs src/FrontEnd/Lex/*.hs $(HSFILES)
	$(HC) $(GHCOPTS)  $(CFILES) --make $< -o $@

lint:
	perl utils/hslint.prl `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`
hlint:
	hlint `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`

.PHONY: ho-clean hl-clean built-sources i manual man manual_ja

ho-clean:
	rm -f -- `find -name \*.ho -print`
	rm -f -- `find ~/.jhc/cache -name \*.ho -print`
hl-clean:
	rm -f -- `find -name \*.hl -print`

clean-local:
	find . \( \! -path \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) \) -print | xargs rm -f --

MAINTAINERCLEANFILES= $(BUILT_SOURCES) $(DRIFTFILES)

CLEANFILES= $(JHC_LIBS) $(JHC_EXT_LIBS) HelloWorld Primes src/StringTable/Atom.hs src/RawFiles.hs

%.pdf: %.ps
	epstopdf $< --outfile $@

%.ps : %.dot
	dot $< -Tps -o$@

%.pdf : %.dot
	dot $< -Tpdf -o$@

#%.html: %.mkd
#	pandoc $< -s -f markdown -t html -o $@

%.shtml: %.mkd
	cat $(srcdir)/docs/template/my_header.shtml > $@
	pandoc $< -f markdown -t html >> $@
	cat $(srcdir)/docs/template/my_footer.shtml >> $@

PUBLISH_DIR=/home/john/public_html/computer/jhc
PUBLISH_MKDS=docs/ext/*.mkd

docs/ext/%.html: docs/ext/%.mkd md5css.txt
	pandoc $< --toc -s -f markdown -t html -s -c `cat md5css.txt` -o $@

publish_ext: $(PUBLISH_MKDS)
	for a in $(PUBLISH_MKDS); do \
		BN=tmp/`basename $$a .mkd`; \
		pandoc $$a --toc -s -f markdown -t html -s -c `cat md5css.txt` -o $$BN.html; \
		cp $$BN.html $(PUBLISH_DIR);  \
	done

publish: docs/building.shtml docs/big-picture.pdf docs/development.shtml docs/index.shtml docs/jhc.shtml docs/manual.html docs/manual.css md5css.txt
	cp -- docs/manual.css $(PUBLISH_DIR)/`cat md5css.txt`
	cp -- $^ $(PUBLISH_DIR)

md5css.txt: docs/manual.css
	echo `md5sum $< | cut -f 1 -d' '`.css > $@
	ln -s manual.css docs/`cat $@`

manual.mkd: $(JHC_MANUAL_FILES)
	perl $(srcdir)/utils/stitch.prl $^ > $@

manual: manual.mkd md5css.txt
	pandoc $< --toc -s -f markdown -t html -s -c `cat md5css.txt` -o docs/$@.html
	sed -e '/^<col width/d' -i docs/$@.html

updatepo: manual.mkd
	po4a-updatepo -M utf8 -f text -m manual.mkd -p po/ja.po

manual_ja.mkd: manual.mkd po/ja.po
	po4a-translate -M utf8 -f text -m manual.mkd -p po/ja.po -l manual_ja.mkd

manual_ja: manual_ja.mkd
	pandoc $< --toc -s -f markdown -t html -s -c `cat md5css.txt` -o $@.html

$(firstword $(JHC_MANUAL_FILES)): utils/stitch.prl docs/man_header.mkd
	touch $@

jhc_man.mkd: $(JHC_MANUAL_FILES)
	perl $(srcdir)/utils/stitch.prl -h $(srcdir)/docs/man_header.mkd -c Using,Options $^ > $@

man: jhc_man.mkd
	pandoc $< -s -f markdown -t man -s  -o jhc.1

options.mkd: drift_processed/Options.hs | jhc
	echo "{-@Options 1" > $@
	echo "" >> $@
	./jhc --help -v  >> $@

fix-whitespace:
	find src -not \( -name .svn -prune -o -name .git -prune \) -type f \( -name \*.hs -o -name \*.\[ch\] \)  -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'
	find rts -not \( -name .svn -prune -o -name .git -prune \) -type f \( -name \*.hs -o -name \*.\[ch\] \)  -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'
	find lib -not \( -name .svn -prune -o -name .git -prune \) -type f \( -name \*.hs -o -name \*.\[ch\] \)  -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'

jhc.1 :
	$(MAKE) man

docs/manual.html :
	$(MAKE) manual

.PHONY: libs
libs: $(JHC_LIBS) $(JHC_EXT_LIBS)

show_elibs: lib/ext/*.yaml
	-perl utils/build_extlibs.prl -l $^

%: examples/%.hs jhc $(JHC_LIBS)
	./jhc -L- -L$(srcdir) -L. --no-cache $(JHC_TEST) $< -o $@ # 2>&1 | tee $@.log

%.hs: %.x
	alex -g $< -o $@
	-sed -i -e 's/^\(ignorePendingBytes (p,c,ps,s) = \).*/\1 (p,c,[],s)/' $@

%.hs: %.y
	happy -a -g -c $< -o $@

%: %.m4
	m4 -P src/data/prelude.m4 $< > $@

%.y: %.y.pp $(srcdir)/utils/preproc.prl
	perl $(srcdir)/utils/preproc.prl  $< > $@

%.hs: %.pp.hs
	perl $(srcdir)/utils/preproc.prl  $< > $@

TESTS=HelloWorld Primes

JHC_MANUAL_FILES = docs/make.mkd options.mkd docs/dependency_format.mkd docs/pragmas.mkd docs/differences.mkd \
	docs/unboxed.mkd rts/rts/jhc_rts.c src/FlagOpts.mkd src/FlagDump.mkd src/E/PrimOpt.hs \
	drift_processed/E/TypeCheck.hs drift_processed/Options/Type.hs docs/libraries.mkd
