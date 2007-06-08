

GHCOPTS= -fglasgow-exts -i.. -i../FrontEnd -O -ignore-package lang

HC= ghc

all: sha1 TestParse

%: %.hs
	$(HC) $(GHCOPTS) --make $< -o $@
