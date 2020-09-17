# Convert graphviz diagrams to PDF for vignettes

diagrams := $(patsubst scripts/diagrams/%.gv, \
                       vignettes/images/%.pdf, \
                       $(wildcard scripts/diagrams/*.gv) \
             )

vignettes := $(patsubst vignettes/%.Rnw, \
                        vignettes/%.tex, \
                        $(wildcard vignettes/*.Rnw) \
              )

all: $(diagrams) data/RNAseq123.RData $(vignettes)

vignettes/images/%.pdf: scripts/diagrams/%.gv
	mkdir -p vignettes/images/
	dot -Tpdf $< > $@

data/RNAseq123.RData: scripts/RNAseq123.R
	Rscript $<

# Work in progress. Need to figure out best way to deal with directories.
vignettes/%.tex: vignettes/%.Rnw
	Rscript -e 'utils::Sweave("$<")'
	mv `basename $@` $@
	pdflatex $@

.PHONY: clean

clean:
	rm --force vignettes/images/*pdf \
	           vignettes/*log \
	           vignettes/*pdf \
	           vignettes/*synctex.gz \
	           vignettes/*tex \
	           vignettes/*toc
