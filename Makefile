# Convert graphviz diagrams to PDF for vignettes

diagrams := $(patsubst scripts/diagrams/%.gv, \
                       vignettes/images/%.pdf, \
                       $(wildcard scripts/diagrams/*.gv) \
             )

all: $(diagrams)

vignettes/images/%.pdf: scripts/diagrams/%.gv
	mkdir -p vignettes/images/
	dot -Tpdf $< > $@

.PHONY: clean

clean:
	rm --force vignettes/images/*pdf
