

GHCOPTS= -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts -i -i../drift_processed -i../drift_processed/FrontEnd -i.. -i../FrontEnd -O -ignore-package lang  -optc-std=c99 -optc-g

HC= ghc

all: sha1 TestParse cff SelfTest


%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@ ../StringTable/StringTable_cbits.c
