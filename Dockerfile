FROM opencpu/base:2.2.2

COPY . /app

WORKDIR /app

RUN Rscript scripts/install-dependencies.R

RUN R CMD build --no-manual --no-build-vignettes .

RUN R CMD INSTALL --no-docs --no-multiarch --no-staged-install OmicNavigator_*.tar.gz

RUN Rscript -e 'OmicNavigator::installApp()'

RUN Rscript scripts/install-test-study.R
