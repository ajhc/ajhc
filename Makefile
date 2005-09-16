JHC_VERSION=0.1

all:   jhc

GHCDEBUGOPTS= -W -fno-warn-unused-matches -fno-warn-unused-binds    # -O2 -ddump-simpl-stats -ddump-rules
GHCINC=  -iFrontEnd
PACKAGES= -package mtl  -package unix -package QuickCheck  #  -prof -auto-all
GHCOPTS=   -O -ignore-package lang  -pgmF drift-ghc  -F $(GHCDEBUGOPTS) $(GHCINC) $(PACKAGES) -fwarn-type-defaults   -fallow-undecidable-instances  -fglasgow-exts -fallow-overlapping-instances

HC = ghc
HC_OPTS = $(GHCOPTS)

DRIFT= ../DrIFT/src/DrIFT

ALLHS:=$(shell find . Grin Boolean Doc C E  FrontEnd DerivingDrift -maxdepth 1 -follow \( -name \*.hs -or -name \*.lhs \) -and \( \! -name Try\*.hs \) | sed -e 's@^\./@@')

BUILTSOURCES= PrimitiveOperators.hs RawFiles.hs FrontEnd/HsParser.hs FlagDump.hs FlagOpts.hs Version.hs VersionCtx.hs


# OBJS is defined in 'depend.make'
# OBJS=$(shell perl ./collect_deps.prl Main.o < depend.make)
#
-include depend.make


SUFFIXES= .hs .lhs .o .hi .hsc .c .h .ly .hi-boot .hs-boot .o-boot


MAIN=Main.hs

%.o: %.hs
	$(HC) -i.  $(HCFLAGS) $(GHCOPTS) -o $@ -c $<
%.o: %.lhs
	$(HC) -i.  $(HCFLAGS) $(GHCOPTS) -o $@ -c $<

%.hi: %.o
	@:

%.hi-boot: %.o-boot
	@:

%.o-boot: %.hs-boot
	$(HC) $(HCFLAGS) $(GHCOPTS) -c $<


jhc: $(OBJS)
	$(HC) $(GHCOPTS) $(EXTRAOPTS) $(OBJS) -o $@

tags: $(ALLHS)
	hasktags $(ALLHS)

regress: jhc Try-Regress.hs
	time ./regress_test.prl try/Try-Regress.hs
	time ./regress_test.prl try/Try-Foo.hs
	time ./regress_test.prl try/Try-Lam.hs
	time ./regress_test.prl try/Try-Case.hs
#	$(MAKE) -C regress
#	(cd regress; ./regress)
#
#

hsdocs:
	haddock -h $(filter-out SelfTest.hs %/HsParser.hs FrontEnd/Representation.hs C/Gen.hs DData/% E/Subst.hs, $(OBJS:.o=.hs)) -o hsdocs

printos:
	echo $(ALLHS)
	echo $(OBJS)


depend: depend.make

depend.make: $(BUILTSOURCES) $(ALLHS)
	$(HC) -M -optdep-f -optdepdepend.make $(HC_OPTS) Main.hs
	sed -e '/^#.*DELETE: End/q' -i depend.make
	echo OBJS=`perl ./collect_deps.prl Main.o < depend.make ` >> depend.make

# $(ALLHS)

clean:
	rm -f $(OBJS) jhc *.hs_code.c `find . -name \*.hi -or -name \*.o-boot -or -name \*.hi-boot`


realclean: clean
	rm -f $(BUILTSOURCES) depend.make

builtfiles: $(BUILTSOURCES)
clean-ho:
	rm -f -- `find -name \*.ho`


# Various rules for generated Haskell files

%.hs: %.flags  ./opt_sets.prl
	perl ./opt_sets.prl -n $< $<  > $@

PrimitiveOperators.hs: op_process.prl data/operators.txt data/primitives.txt data/PrimitiveOperators-in.hs
	perl ./op_process.prl > $@ || rm -f $@

RawFiles.hs:  data/HsFFI.h data/jhc_rts.c
	perl ./op_raw.prl $(basename $@)  $^ > $@

FrontEnd/HsParser.hs: FrontEnd/HsParser.ly
	happy -a -g -c FrontEnd/HsParser.ly

VersionCtx.hs: _darcs/inventory
	darcs changes --context > changes.txt  || echo "No darcs Context Available!" > changes.txt
	perl ./op_raw.prl $(basename $@) changes.txt > $@
	rm -f changes.txt

Version.hs: _darcs/inventory
	echo "module Version where"                                     > $@
	echo "jhcVersion = \"$(JHC_VERSION)\""                         >> $@
	date +'compileDate = "%Y%m%d"'                                 >> $@
	darcs changes -t '.' \
	|  perl -e '<>;$$_=<>;s/^\s*tagged\s+/darcsTag = "/;s/$$/"/;print' >> $@
	darcs changes --from-tag='' --xml-output | grep '</patch>' \
	| wc -l | perl -e 'print "darcsPatches = \"".(<>-1)."\"\n"'    >> $@

.PHONY: depend clean realclean builtfiles clean-ho  regress hsdocs

