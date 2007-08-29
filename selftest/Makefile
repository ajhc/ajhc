

GHCOPTS= -fglasgow-exts -i.. -i../FrontEnd -O -ignore-package lang

HC= ghc

all: sha1 TestParse cff

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@
