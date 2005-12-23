JHC_VERSION=0.1

all: jhc jhcp

GHCDEBUGOPTS= -W -fno-warn-unused-matches -fno-warn-unused-binds    # -O2 -ddump-simpl-stats -ddump-rules
GHCPROFOPTS=   -prof -auto-all -osuf prof.o -hisuf prof.hi
GHCINC=  -iFrontEnd
PACKAGES= -package mtl  -package unix -package QuickCheck
GHCOPTS=   -O -ignore-package lang  -pgmF drift-ghc  -F $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) -fwarn-type-defaults   -fallow-undecidable-instances  -fglasgow-exts -fallow-overlapping-instances

HC = ghc
HCI = ghci
HC_OPTS = $(GHCOPTS)

BUILTSOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs FlagOpts.hs Version/Raw.hs Version/Ctx.hs

# HSFILES is defined here, it can be updated with 'make depend' whenever a new source file is added
-include depend.make


SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot


MAIN=Main.hs

jhcp: $(BUILTSOURCES) $(HSFILES)
	date '+%y%m%d%H%M.%S' > /tmp/$@.date.tmp
	$(HC) $(GHCOPTS) $(EXTRAOPTS) $(GHCPROFOPTS) --make $(MAIN) -o $@
	touch -t `cat /tmp/$@.date.tmp` $@

jhc: $(BUILTSOURCES) $(HSFILES)
	date '+%y%m%d%H%M.%S' > /tmp/$@.date.tmp
	$(HC) $(GHCOPTS) $(EXTRAOPTS) --make $(MAIN) -o $@
	touch -t `cat /tmp/$@.date.tmp` $@

i:
	ghci $(GHCOPTS) $(EXTRAOPTS) Main.hs


tags: $(HSFILES)
	hasktags $(HSFILES)

regress: jhc
	time ./utils/regress_test.prl test/Try-Regress.hs
	time ./utils/regress_test.prl test/Try-Foo.hs
	time ./utils/regress_test.prl test/Try-Lam.hs
	time ./utils/regress_test.prl test/Try-Case.hs

hsdocs:
	haddock -h $(filter-out DataConstructors.hs SelfTest.hs %/HsParser.hs FrontEnd/Representation.hs C/Gen.hs , $(HSFILES)) -o hsdocs

printos:
	echo $(HSFILES)


depend: $(BUILTSOURCES)
	$(HC) -M -optdep-f -optdepdep.tmp $(HC_OPTS) $(MAIN)
	echo HSFILES=`egrep -o '[A-Za-z/.]+.hs' dep.tmp | sed -e 's/^\.\///' | sort` > depend.make
	rm -f dep.tmp

clean:
	rm -f  jhc jhcp *.hs_code.c `find . -name \*.hi -or -name \*.o-boot -or -name \*.hi-boot -or -name \*.o`


realclean: clean
	rm -f $(BUILTSOURCES) depend.make

builtfiles: $(BUILTSOURCES)

clean-ho:
	rm -f -- `find -name \*.ho`


# Various rules for generated Haskell files

%.hs: %.flags  ./utils/opt_sets.prl
	perl ./utils/opt_sets.prl -n $< $<  > $@

PrimitiveOperators.hs: utils/op_process.prl data/operators.txt data/primitives.txt data/PrimitiveOperators-in.hs
	perl ./utils/op_process.prl > $@ || rm -f $@

RawFiles.hs:  data/HsFFI.h data/jhc_rts.c
	perl ./utils/op_raw.prl $(basename $@)  $^ > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.ly
	happy -a -g -c FrontEnd/HsParser.ly

Version/Ctx.hs: _darcs/inventory
	darcs changes --context > changes.txt  || echo "No darcs Context Available!" > changes.txt
	perl ./utils/op_raw.prl Version.Ctx changes.txt > $@
	rm -f changes.txt

Version/Raw.hs: _darcs/inventory
	echo "module Version.Raw where"                                > $@
	echo "jhcVersion = \"$(JHC_VERSION)\""                         >> $@
	date +'compileDate = "%Y%m%d"'                                 >> $@
	darcs changes -t '.' \
	|  perl -e '<>;$$_=<>;s/^\s*tagged\s+/darcsTag = "/;s/$$/"/;print' >> $@
	darcs changes --from-tag='' --xml-output | grep '</patch>' \
	| wc -l | perl -e 'print "darcsPatches = \"".(<>-1)."\"\n"'    >> $@

.PHONY: depend clean realclean builtfiles clean-ho  regress hsdocs i

