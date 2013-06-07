#!/usr/bin/make -f
ODGS := $(wildcard *.odg)
PNGS := $(patsubst %.odg,%.png,${ODGS})

all: ${PNGS}

%.png: %.odg
	unoconv -n -f png -o $@.tmp $< 2> /dev/null   || \
          unoconv -f png -o $@.tmp $<                 || \
	  unoconv -n -f png -o $@.tmp $< 2> /dev/null || \
          unoconv -f png -o $@.tmp $<
	convert -resize 640x $@.tmp $@
	rm -f $@.tmp

clean:
	rm -f *.tmp

distclean:
	rm -f *.png *.tmp

.PHONY: clean distclean
