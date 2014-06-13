INC= -i -i. -i../drift_processed -i../src -i../compat/haskell98
GHCOPTS= -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts \
	 -XBangPatterns $(INC) -XTypeFamilies -XViewPatterns -XRecordWildCards \
	 -O -package QuickCheck -ignore-package lang  -optc-std=c99 -optc-g \
	-XTypeFamilies -XViewPatterns -XUndecidableInstances -XOverlappingInstances \
	-XRecordWildCards -XRecursiveDo -XTupleSections -XParallelListComp \
	-XGeneralizedNewtypeDeriving -XScopedTypeVariables -XFlexibleInstances \
	-XTypeSynonymInstances -XMultiParamTypeClasses -XDeriveDataTypeable \
	-fglasgow-exts -XNoMonoLocalBinds -XNamedFieldPuns  -I../src \
	-odir obj -hidir obj -W -O2

HC= ghc

TESTPROGS=md5 sha1 TestParse cff SelfTest UnionSolve TempDir comptest
all: $(TESTPROGS)

UnionSolve: UnionSolve.hs ../src/Util/UnionSolve.hs
	ghc $(INC) --make $<

TempDir: TempDir.hs ../src/Support/TempDir.hs

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@  -I../src/cbits ../src/cbits/md5sum.c ../src/StringTable/StringTable_cbits.c ../src/cbits/lookup3.c

SelfTest: SelfTest.hs OptionsTest.hs AtomName.hs

clean:
	rm -f -- *.o *.hi $(TESTPROGS)
