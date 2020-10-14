# Create vignettes

# Variables --------------------------------------------------------------------

diagrams := $(patsubst scripts/diagrams/%.gv, \
                       vignettes/images/%.pdf, \
                       $(wildcard scripts/diagrams/*.gv) \
             )

data := data/RNAseq123.RData

vignettes := $(patsubst vignettes/%.Rnw, \
                        vignettes/%.tex, \
                        $(wildcard vignettes/*.Rnw) \
              )

# Targets ----------------------------------------------------------------------

all: diagrams data vignettes

diagrams: $(diagrams)

data: $(data)

vignettes: $(vignettes)

guide: vignettes/OmicNavigatorUsersGuide.tex

api: vignettes/OmicNavigatorAPI.tex

# Rules ------------------------------------------------------------------------

# Convert graphviz diagrams to PDF for vignettes
vignettes/images/%.pdf: scripts/diagrams/%.gv
	mkdir -p vignettes/images/
	dot -Tpdf $< > $@

# Run RNAseq123 analysis, subset objects, and save them for use in User's Guide
$(data): scripts/RNAseq123.R
	Rscript $<

# Weave vignettes to LaTeX and compile to PDF
vignettes/%.tex: vignettes/%.Rnw $(diagrams) $(data)
	cd vignettes; Rscript -e 'utils::Sweave(basename("$<"))'
	cd vignettes; Rscript -e 'tools::texi2pdf(basename("$@"))'

.PHONY: clean

clean:
	rm --force vignettes/images/*pdf \
	           vignettes/*aux \
	           vignettes/*bbl \
	           vignettes/*blg \
	           vignettes/*log \
	           vignettes/*out \
	           vignettes/*pdf \
	           vignettes/*synctex.gz \
	           vignettes/*tex \
	           vignettes/*toc
