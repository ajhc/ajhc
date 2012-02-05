ACLOCAL_AMFLAGS=-I ac-macros

bin_PROGRAMS = jhc

jhc_SOURCES = src/Main.hs $(HSFILES) $(BUILT_SOURCES) $(HSBOOT) $(DRIFTFILES)

BUILT_SOURCES= src/RtsFiles.hs src/RawFiles.hs src/FrontEnd/HsParser.hs src/FlagDump.hs \
	       src/FlagOpts.hs src/Name/Prim.hs src/Info/Properties.hs $(DRIFTFILES)

MY_CINCLUDES= -I$(srcdir)/src/data -I$(srcdir)/src/StringTable -I$(srcdir)/src/cbits -I$(builddir)/src/cbits
MY_CFLAGS= $(MY_CINCLUDES) -optc-std=c99 -optc-O2 -optc-Wall
CC = $(HC)

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
       	src/Grin/NodeAnalyze.hs src/Grin/Noodle.hs src/Grin/Optimize.hs src/Grin/Show.hs src/Grin/Val.hs \
	src/Grin/Whiz.hs src/Ho/Binary.hs src/Ho/Collected.hs src/Ho/Library.hs src/Info/Binary.hs src/Info/Info.hs src/Info/Properties.hs \
	src/Info/Types.hs src/Interactive.hs src/Main.hs src/Name/Binary.hs src/Name/Id.hs src/Name/Name.hs src/Name/Names.hs \
	src/Name/Prim.hs src/PackedString.hs src/Ho/ReadSource.hs \
	src/RawFiles.hs src/RtsFiles.hs src/Stats.hs src/Support/CanType.hs src/Support/CFF.hs src/Support/Compat.hs \
	src/Support/FreeVars.hs src/Support/MapBinaryInstance.hs src/Support/MD5.hs src/Support/Tickle.hs src/Support/Transform.hs \
	src/Support/Tuple.hs src/Support/Unparse.hs src/Util/BitSet.hs src/Util/ContextMonad.hs src/Util/FilterInput.hs \
	src/Util/Gen.hs src/Util/Graph.hs src/Util/Graphviz.hs src/Util/HasSize.hs src/Util/Histogram.hs src/Util/Inst.hs \
	src/Util/IntBag.hs src/Util/Interact.hs src/Util/NameMonad.hs src/Util/Once.hs src/Util/Perhaps.hs src/Util/ReaderWriter.hs \
	src/Util/Relation.hs src/Util/RWS.hs src/Util/SameShape.hs src/Util/Seq.hs src/Util/SetLike.hs src/Util/UnionFind.hs \
	src/Util/UnionSolve.hs src/Util/UniqueMonad.hs src/Util/Util.hs src/Util/VarName.hs src/Version/Config.hs src/Version/Version.hs \
	src/Support/IniParse.hs src/E/Lint.hs src/Util/Progress.hs src/Grin/StorageAnalysis.hs src/Util/YAML.hs src/Grin/Main.hs \
        src/E/Main.hs src/Util/GMap.hs src/Util/ExitCodes.hs src/Support/TempDir.hs src/E/PrimDecode.hs

GHCDEBUGOPTS= -W -fno-warn-unused-matches  -fwarn-type-defaults
GHCPROFOPTS= -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCSELFTESTOPTS= -osuf test.o -hisuf test.hi -DEXPORT_SELFTEST=,_selftest
GHCNONSELFTESTOPTS= -DEXPORT_SELFTEST=
GHCINC=  -i -i$(srcdir)/drift_processed \
	 -i$(srcdir)/src -i$(builddir)/src -odir $(builddir)/src -hidir $(builddir)/src

PACKAGES= -package fgl -package regex-compat -package random -package array -package directory \
          -package bytestring -package binary -package pretty -package mtl -package containers \
	  -package unix -package haskell98 -package utf8-string -package zlib -package HsSyck \
	  -package filepath -package process

GHCLANG= -XTypeFamilies -XViewPatterns -XUndecidableInstances -XOverlappingInstances \
	-XRecordWildCards -XRecursiveDo -XTupleSections -XParallelListComp \
	-XGeneralizedNewtypeDeriving -XScopedTypeVariables -XFlexibleInstances \
	-XTypeSynonymInstances -XMultiParamTypeClasses -XDeriveDataTypeable \
	-fglasgow-exts -XNoMonoLocalBinds -XNamedFieldPuns

GHCOPTS= $(HSOPTS) -XBangPatterns -O @GHCFLAGS@ $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) $(GHCLANG)

JHC_LIBS = jhc-1.0.hl base-1.0.hl haskell98-1.0.hl applicative-1.0.hl flat-foreign-1.0.hl jhc-prim-1.0.hl

EXTRA_DIST = src/data rts utils docs src/FrontEnd/HsParser.y $(BUILT_SOURCES) \
             lib/jhc lib/base lib/haskell98 lib/flat-foreign $(JHC_LIBS) $(JHC_EXT_LIBS) \
	     src/StringTable/StringTable_cbits.c src/StringTable/StringTable_cbits.h src/cbits/md5sum.c jhc.1 \
	     src/StringTable/Atom.hsc jhc.spec src/cbits/lookup3.c \
	     manual.html lib/applicative examples/HelloWorld.hs examples/Calendar.hs \
	     src/FlagOpts.flags src/FlagDump.flags lib/ext

# we recreate the manual and man page here just to ensure they are up to date
dist-hook: man manual
	cp $(srcdir)/manual.html $(distdir)/manual.html
	cp $(srcdir)/jhc.1 $(distdir)/jhc.1
	find $(distdir) \( -name _darcs -o -name \*.ho \) -print | xargs rm -rf --

all-local: $(JHC_LIBS)

install-data-hook: $(JHC_LIBS) $(JHC_EXT_LIBS)
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@
	$(INSTALL) -d $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
	$(INSTALL) -d $(DESTDIR)$(sysconfdir)/@PACKAGE@-@SHORTVERSION@
	for lib in $(JHC_LIBS) $(JHC_EXT_LIBS); do \
		test -f "$$lib" && $(INSTALL_DATA) $$lib $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
		test -f "$(srcdir)/$$lib" && $(INSTALL_DATA) "$(srcdir)/$$lib" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@ ; \
	done
	$(INSTALL_DATA) "$(srcdir)/src/data/HsFFI.h" $(DESTDIR)$(datadir)/@PACKAGE@-@SHORTVERSION@/include
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

RAWFILES = src/data/ViaGhc.hs ChangeLog \
	   src/data/shortchange.txt src/data/prelude.m4 src/data/targets.ini

RTSFILES = src/data/HsFFI.h rts/wsize.h rts/jhc_rts_header.h rts/jhc_jgc.h \
	   rts/debug.c rts/jhc_rts_alloc.c rts/jhc_rts.c rts/profile.c rts/jhc_rts2.c rts/bitarray.h \
	   rts/slub.c rts/jhc_jgc.c  rts/lib_cbits.c

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

CFILES = src/StringTable/StringTable_cbits.o src/cbits/md5sum.o src/cbits/lookup3.o

%.o : %.c
	mkdir -p `echo $@ | sed -e 's@/[^/]*$$@@'`
	$(HC) $(MY_CFLAGS) $< -c -o $@

jhc: src/Main.hs  $(HSFILES) $(BUILT_SOURCES) src/StringTable/Atom.hs $(CFILES)
	$(BUILD_STOW) $(HC) $(GHCOPTS) --make $< $(CFILES) -o $@

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
	perl $(srcdir)/utils/opt_sets.prl -n $< $< > $@

%.mkd: %.flags  utils/opt_sets.prl
	perl $(srcdir)/utils/opt_sets.prl -f f -m -n $< $< > $@

src/Info/Properties.hs: src/data/props.txt utils/gen_props.prl
	perl $(srcdir)/utils/gen_props.prl $< > $@ || rm -f $@

src/Name/Prim.hs: utils/op_names.prl src/data/names.txt
	perl $< > $@ || rm -f $@

src/RawFiles.hs: utils/op_raw.prl $(RAWFILES)
	perl $< $(basename $@) $(RAWFILES) > $@

src/RtsFiles.hs: utils/op_raw.prl $(RTSFILES)
	perl $< -c $(basename $@) $(RTSFILES) > $@

src/FrontEnd/HsParser.hs: src/FrontEnd/HsParser.y
	happy -a -g -c $< -o $@

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

.INTERMEDIATE: deps.txt jhc_man.mkd  options.mkd
.DELETE_ON_ERROR: deps.txt depend.make ChangeLog data/shortchange.txt manual.mkd jhc_man.mkd options.mkd

deps.txt:
	$(MAKE) $(AM_MAKEFLAGS) $(BUILT_SOURCES)
	$(HC) -M -optdep-f -optdep$@ $(GHCOPTS) src/Main.hs

depend.make: deps.txt
	echo HSFILES=`egrep -o '[A-Za-z0-9/._]+.hs' $< | sed -e 's/^\.\///' | sort | uniq` > depend.make

built-sources: $(BUILT_SOURCES)

i: $(BUILT_SOURCES)
	$(HC) --interactive $(GHCOPTS) src/Main.hs $(CFILES)

lint:
	perl utils/hslint.prl `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`
hlint:
	hlint `darcs wh -s | sed -e 's/^..//' -e 's/ .*$$//' | grep '.hs$$'`

.PHONY: ho-clean hl-clean built-sources i manual man

ho-clean:
	rm -f -- `find -name \*.ho -print`
	rm -f -- `find ~/.jhc/cache -name \*.ho -print`
hl-clean:
	rm -f -- `find -name \*.hl -print`

clean-local:
	find . \( \! -path \*/_\* \( -name \*.o-boot -o -name \*.hi -o -name \*.o -o -name \*.hi-boot \) \) -print | xargs rm -f --

MAINTAINERCLEANFILES= $(BUILT_SOURCES) $(DRIFTFILES)

CLEANFILES= $(JHC_LIBS) $(JHC_EXT_LIBS)

LIB_OPTIONS=$(RTSOPTS) $(JHC_TEST)  -L- -L.

jhc-prim-1.0.hl: lib/jhc-prim/jhc-prim.yaml
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
jhc-1.0.hl: lib/jhc/jhc.yaml jhc-prim-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
base-1.0.hl: lib/base/base.yaml jhc-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
applicative-1.0.hl: lib/applicative/applicative.yaml base-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
haskell98-1.0.hl: lib/haskell98/haskell98.yaml base-1.0.hl
	./jhc $(LIB_OPTIONS) --build-hl $< -o $@
flat-foreign-1.0.hl: lib/flat-foreign/flat-foreign.cabal base-1.0.hl
	./jhc $(LIB_OPTIONS) -ilib/flat-foreign -pjhc -pbase --build-hl $< -o $@

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

publish: docs/building.shtml docs/big-picture.pdf docs/development.shtml docs/index.shtml docs/jhc.shtml manual.html docs/manual.css
	cp -- $^ /home/john/public_html/computer/jhc

manual.mkd: utils/stitch.prl $(JHC_MANUAL_FILES)
	perl utils/stitch.prl $(JHC_MANUAL_FILES) > $@

manual: manual.mkd
	pandoc $< --toc -s -f markdown -t html -s -c manual.css -o $@.html

jhc_man.mkd: utils/stitch.prl docs/man_header.mkd $(JHC_MANUAL_FILES)
	perl utils/stitch.prl -h docs/man_header.mkd -c Using,Options $(JHC_MANUAL_FILES) > jhc_man.mkd

man: jhc_man.mkd
	pandoc jhc_man.mkd -s -f markdown -t man -s  -o jhc.1

options.mkd: jhc
	echo "{-@Options 1" > $@
	./jhc --help | sed -e 's/^/    /'  >> $@

fix-whitespace:
#	#find src -not \( -name .svn -prune -o -name .git -prune \) -type f -print0 | xargs -0 sed -i  -E 's/[[:space:]]*$$//'
#	#find lib -not \( -name .svn -prune -o -name .git -prune \) -type f -print0 | xargs -0 sed -i  -E 's/[[:space:]]*$$//'
	find src -not \( -name .svn -prune -o -name .git -prune \) -type f -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'
	find lib -not \( -name .svn -prune -o -name .git -prune \) -type f -print0 | xargs -0 sed -i  -E 's/[ 	]*$$//'

jhc.1 :
	$(MAKE) man

manual.html :
	$(MAKE) manual

.PHONY: libs
libs: $(JHC_LIBS) $(JHC_EXT_LIBS)

show_elibs: lib/ext/*.cabal  lib/ext/*.yaml
	-perl utils/build_extlibs.prl -l $^

%: examples/%.hs jhc
	./jhc -L- -L. $(JHC_TEST) $< -o $@ 2>&1 | tee $@.log

## autogenerated dependencies

applicative-1.0.hl: lib/applicative/applicative.yaml lib/applicative/Control/Arrow.hs lib/applicative/Control/Applicative.hs lib/applicative/Data/Foldable.hs \
    lib/applicative/Control/Category.hs lib/applicative/Data/Traversable.hs base-1.0.hl jhc-1.0.hl
base-1.0.hl: lib/base/base.yaml lib/base/Debug/Trace.hs lib/base/Data/Array.hs \
    lib/base/Data/Typeable.hs lib/base/Text/Printf.hs lib/base/System/Directory.hs lib/base/Data/Maybe.hs \
    lib/base/System/IO/Error.hs lib/base/System/IO/Binary.hs lib/base/System/CPUTime.hs lib/base/System/IO/Pipe.hs \
    lib/base/Control/Monad/Instances.hs lib/base/Foreign/Marshal/Error.hs lib/base/System/Exit.hs lib/base/System/Environment.hs \
    lib/base/Data/Array/IO.hs lib/base/Data/Version.hs lib/base/Text/Show/Functions.hs lib/base/Control/Monad.hs \
    lib/base/Data/Functor.hs lib/base/Data/Monoid.hs lib/base/System/Locale.hs lib/base/System/IO.hs \
    lib/base/Foreign/Marshal/Pool.hs lib/base/Control/Monad/Fix.hs lib/base/Data/Complex.hs lib/base/Data/IORef.hs \
    lib/base/System/Console/GetOpt.hs lib/base/Data/List.hs lib/base/System/Cmd.hs lib/base/System/Random.hs \
    lib/base/Data/Unicode.hs lib/base/Foreign/ForeignPtr.hs lib/base/System/Time.hs lib/base/Control/Exception.hs \
    lib/base/Foreign/StablePtr.hs lib/base/Foreign/C.hs lib/base/Data/Bits.hs lib/base/Foreign/Marshal.hs \
    lib/base/Foreign.hs lib/base/Data/Array/Unboxed.hs lib/base/Data/Function.hs lib/base/Data/Ix.hs \
    lib/base/System/Info.hs jhc-1.0.hl
flat-foreign-1.0.hl: lib/flat-foreign/flat-foreign.cabal lib/flat-foreign/StablePtr.hs lib/flat-foreign/MarshalAlloc.hs lib/flat-foreign/CForeign.hs \
    lib/flat-foreign/Storable.hs lib/flat-foreign/Ptr.hs lib/flat-foreign/MarshalArray.hs lib/flat-foreign/Int.hs \
    lib/flat-foreign/CString.hs lib/flat-foreign/Word.hs lib/flat-foreign/CError.hs lib/flat-foreign/CTypes.hs \
    lib/flat-foreign/MarshalError.hs lib/flat-foreign/MarshalUtils.hs lib/flat-foreign/Bits.hs lib/flat-foreign/ForeignPtr.hs \
    base-1.0.hl jhc-1.0.hl
haskell98-1.0.hl: lib/haskell98/haskell98.yaml lib/haskell98/System.hs lib/haskell98/List.hs lib/haskell98/Time.hs \
    lib/haskell98/Array.hs lib/haskell98/Random.hs lib/haskell98/Complex.hs lib/haskell98/Locale.hs \
    lib/haskell98/CPUTime.hs lib/haskell98/Ratio.hs lib/haskell98/Monad.hs lib/haskell98/Directory.hs \
    lib/haskell98/IO.hs lib/haskell98/Ix.hs lib/haskell98/Char.hs lib/haskell98/Maybe.hs \
    base-1.0.hl jhc-1.0.hl
jhc-1.0.hl: lib/jhc/jhc.yaml lib/jhc/Jhc/Prim.hs lib/jhc/Jhc/IO.hs lib/jhc/Jhc/Int.hs lib/jhc/Jhc/Inst/Enum.hs \
    lib/jhc/Data/Int.hs lib/jhc/Prelude/IO.hs lib/jhc/Jhc/Order.hs lib/jhc/System/C/Stdio.hs lib/jhc/Data/Word.hs \
    lib/jhc/Foreign/C/String.hs lib/jhc/Jhc/Show.hs lib/jhc/Jhc/Handle.hs lib/jhc/Jhc/Addr.hs lib/jhc/Numeric.hs \
    lib/jhc/Jhc/Float.hs lib/jhc/Jhc/Inst/Storable.hs lib/jhc/Jhc/Inst/Num.hs lib/jhc/Foreign/C/Types.hs lib/jhc/Data/Ratio.hs \
    lib/jhc/Data/Char.hs lib/jhc/Jhc/List.hs lib/jhc/Jhc/Type/Float.hs lib/jhc/Foreign/Marshal/Utils.hs lib/jhc/Jhc/Basics.hs \
    lib/jhc/System/Mem.hs lib/jhc/Jhc/Type/Basic.hs lib/jhc/Prelude.hs lib/jhc/Jhc/Num.hs lib/jhc/Jhc/Text/Read.hs \
    lib/jhc/Jhc/Type/Ptr.hs lib/jhc/Foreign/C/Error.hs lib/jhc/Foreign/Storable.hs lib/jhc/Foreign/Ptr.hs lib/jhc/Jhc/Inst/Order.hs \
    lib/jhc/Jhc/Enum.hs lib/jhc/Jhc/Type/Handle.hs lib/jhc/Jhc/Tuples.hs lib/jhc/Jhc/String.hs lib/jhc/Prelude/Float.hs \
    lib/jhc/Jhc/Type/C.hs lib/jhc/Jhc/Inst/PrimEnum.hs lib/jhc/Jhc/Inst/Show.hs lib/jhc/Jhc/Array.hs lib/jhc/Prelude/Text.hs \
    lib/jhc/Foreign/Marshal/Alloc.hs lib/jhc/Foreign/Marshal/Array.hs lib/jhc/Jhc/Options.hs lib/jhc/Prelude/CType.hs lib/jhc/Jhc/Monad.hs \
    lib/jhc/System/IO/Unsafe.hs lib/jhc/System/Mem/StableName.hs lib/jhc/Jhc/Inst/Read.hs lib/jhc/Jhc/Maybe.hs jhc-prim-1.0.hl
jhc-prim-1.0.hl: lib/jhc-prim/jhc-prim.yaml lib/jhc-prim/Jhc/Prim/IO.hs lib/jhc-prim/Jhc/Prim/Prim.hs lib/jhc-prim/Jhc/Prim/Wrapper.hs lib/jhc-prim/Jhc/Prim/Bits.hs

containers-0.3.0.0.hl: lib/ext/containers.cabal base-1.0.hl applicative-1.0.hl jhc-1.0.hl lib/ext/containers.patch
	perl utils/build_extlibs.prl $<
Diff-0.1.2.hl: lib/ext/Diff.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
html-1.0.1.2.hl: lib/ext/html.cabal jhc-1.0.hl
	perl utils/build_extlibs.prl $<
HUnit-1.2.2.1.hl: lib/ext/HUnit.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
parsec-2.1.0.1.hl: lib/ext/parsec.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
transformers-0.2.1.0.hl: lib/ext/transformers.cabal base-1.0.hl applicative-1.0.hl
	perl utils/build_extlibs.prl $<
pretty-1.0.1.1.hl: lib/ext/pretty.cabal jhc-1.0.hl
	perl utils/build_extlibs.prl $<
QuickCheck-1.2.0.0.hl: lib/ext/QuickCheck.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
safe-0.2.hl: lib/ext/safe.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
smallcheck-0.4.hl: lib/ext/smallcheck.cabal base-1.0.hl haskell98-1.0.hl jhc-1.0.hl lib/ext/smallcheck.patch
	perl utils/build_extlibs.prl $<
utility-ht-0.0.5.1.hl: lib/ext/utility-ht.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
xhtml-3000.2.0.1.hl: lib/ext/xhtml.cabal base-1.0.hl jhc-1.0.hl
	perl utils/build_extlibs.prl $<
filepath-1.2.0.0.hl: lib/ext/filepath.cabal jhc-1.0.hl base-1.0.hl
	perl utils/build_extlibs.prl $<
deepseq-1.1.0.2.hl: lib/ext/deepseq.cabal jhc-1.0.hl base-1.0.hl applicative-1.0.hl containers-0.3.0.0.hl
	perl utils/build_extlibs.prl $<

JHC_EXT_LIBS = containers-0.3.0.0.hl Diff-0.1.2.hl html-1.0.1.2.hl HUnit-1.2.2.1.hl parsec-2.1.0.1.hl \
    pretty-1.0.1.1.hl QuickCheck-1.2.0.0.hl safe-0.2.hl smallcheck-0.4.hl utility-ht-0.0.5.1.hl \
    xhtml-3000.2.0.1.hl transformers-0.2.1.0.hl filepath-1.2.0.0.hl deepseq-1.1.0.2.hl

JHC_MANUAL_FILES = options.mkd docs/make.mkd docs/dependency_format.mkd docs/pragmas.mkd docs/differences.mkd \
	docs/unboxed.mkd rts/jhc_rts2.c src/FlagOpts.mkd src/FlagDump.mkd src/E/PrimOpt.hs \
	drift_processed/E/TypeCheck.hs drift_processed/Options.hs
