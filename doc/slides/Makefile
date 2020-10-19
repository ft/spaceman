TARGET = whitespace.pdf
PUBLISH = ../whitespace-slides.pdf

GRAPHICS_SVG  = graphics/numbers.svg
GRAPHICS_SVG += graphics/labels.svg
GRAPHICS_SVG += graphics/labels-simple.svg
GRAPHICS_SVG += graphics/op-arith-div.svg
GRAPHICS_SVG += graphics/implementation.svg
GRAPHICS_PDF = ${GRAPHICS_SVG:.svg=.pdf}

DEPENDENCIES = $(GRAPHICS_PDF)

LATEX = lualatex
PANDOC = pandoc
INKSCAPE = inkscape
SVG2PDF = DISPLAY= $(INKSCAPE) --export-type=pdf --export-filename

FILTERS = filters/simg.py
MDWN2X = $(PANDOC) $(foreach f,$(FILTERS),-F $(f))
MAINTEXDOCUMENT = $(patsubst %.pdf,%.tex,$(TARGET))

all: $(DEPENDENCIES) $(TARGET)

.SUFFIXES: .svg .pdf

.svg.pdf:
	$(SVG2PDF) $@ $<

.SUFFIXES: .mdwn .tex

.mdwn.tex:
	$(MDWN2X) -f markdown -t beamer $< -o $@

slides.tex: $(FILTERS) $(DEPENDENCIES) Makefile

$(TARGET): $(MAINTEXDOCUMENT) slides.tex title.tex
	$(LATEX) $(MAINTEXDOCUMENT)
	$(LATEX) $(MAINTEXDOCUMENT)

clean:
	rm -f $(TARGET) slides.tex
	rm -f $(GRAPHICS_PDF)
	rm -f *.aux *.log *.nav *.out *.snm *.toc *.vrb
	rm -f base.pdf base.tex *~
	rm -Rf auto

base.tex:
	$(PANDOC) -t beamer slides.mdwn -o base.tex

base.pdf:
	$(PANDOC) -t beamer slides.mdwn -o base.pdf

base: base.tex base.pdf

publish: $(TARGET)
	cp $(TARGET) $(PUBLISH)

.PHONY: all base clean publish
