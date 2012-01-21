INC= -i -i../drift_processed -i../src
GHCOPTS= -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts \
	 -XBangPatterns $(INC) \
	 -O -package QuickCheck -ignore-package lang  -optc-std=c99 -optc-g

HC= ghc

all: md5 sha1 TestParse cff SelfTest UnionSolve TempDir

UnionSolve: UnionSolve.hs ../src/Util/UnionSolve.hs
	ghc $(INC) --make $<

TempDir: TempDir.hs ../src/Support/TempDir.hs

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@  -I../src/cbits ../src/cbits/md5sum.c ../src/StringTable/StringTable_cbits.c

clean:
	rm -f -- md5 sha1 TestParse cff SelfTest
