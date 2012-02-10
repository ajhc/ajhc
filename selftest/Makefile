INC= -i -i../drift_processed -i../src
GHCOPTS= -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts \
	 -XBangPatterns $(INC) -XTypeFamilies -XViewPatterns -XRecordWildCards \
	 -O -package QuickCheck -ignore-package lang  -optc-std=c99 -optc-g \
	-XTypeFamilies -XViewPatterns -XUndecidableInstances -XOverlappingInstances \
	-XRecordWildCards -XRecursiveDo -XTupleSections -XParallelListComp \
	-XGeneralizedNewtypeDeriving -XScopedTypeVariables -XFlexibleInstances \
	-XTypeSynonymInstances -XMultiParamTypeClasses -XDeriveDataTypeable \
	-fglasgow-exts -XNoMonoLocalBinds -XNamedFieldPuns 

HC= ghc

all: md5 sha1 TestParse cff SelfTest UnionSolve TempDir

UnionSolve: UnionSolve.hs ../src/Util/UnionSolve.hs
	ghc $(INC) --make $<

TempDir: TempDir.hs ../src/Support/TempDir.hs

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@  -I../src/cbits ../src/cbits/md5sum.c ../src/StringTable/StringTable_cbits.c ../src/cbits/lookup3.c

clean:
	rm -f -- md5 sha1 TestParse cff SelfTest
