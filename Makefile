# Convert graphviz diagrams to PDF for vignettes

diagrams := $(patsubst scripts/diagrams/%.gv, \
                       vignettes/images/%.pdf, \
                       $(wildcard scripts/diagrams/*.gv) \
             )

vignettes := $(patsubst vignettes/%.Rnw, \
                        vignettes/%.tex, \
                        $(wildcard vignettes/*.Rnw) \
              )

all: diagrams data vignettes

diagrams: $(diagrams)

data: data/RNAseq123.RData

vignettes: ${vignettes}

vignettes/images/%.pdf: scripts/diagrams/%.gv
	mkdir -p vignettes/images/
	dot -Tpdf $< > $@

data/RNAseq123.RData: scripts/RNAseq123.R
	Rscript $<

vignettes/%.tex: vignettes/%.Rnw
	cd vignettes; Rscript -e 'utils::Sweave(basename("$<"))'
	cd vignettes; Rscript -e 'tools::texi2pdf(basename("$@"))'

.PHONY: clean

clean:
	rm --force vignettes/images/*pdf \
	           vignettes/*aux \
	           vignettes/*log \
	           vignettes/*out \
	           vignettes/*pdf \
	           vignettes/*synctex.gz \
	           vignettes/*tex \
	           vignettes/*toc
