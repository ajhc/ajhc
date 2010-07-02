

GHCOPTS= -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts \
	 -XBangPatterns \
	 -i -i../drift_processed -i../drift_processed/FrontEnd -i../src \
	 -O -package QuickCheck -ignore-package lang  -optc-std=c99 -optc-g

HC= ghc

all: md5 sha1 TestParse cff SelfTest


%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@  -I../src/cbits ../src/cbits/md5sum.c ../src/StringTable/StringTable_cbits.c

clean:
	rm -f -- md5 sha1 TestParse cff SelfTest
