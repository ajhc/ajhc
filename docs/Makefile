

big-picture.pdf: big-picture.ps
	epstopdf $< --outfile $@

big-picture.ps: big-picture.dot
	dot $< -Tps -o$@

publish: big-picture.pdf big-picture.dot
	cp big-picture.pdf big-picture.dot  ~/public_html/computer/jhc


.PHONY: publish
