FROM opencpu/base:v2.2.8

COPY . /app

WORKDIR /app

RUN bash scripts/setup-r2u.sh

RUN bash scripts/install-dependencies-r2u.sh

RUN R CMD build --no-manual --no-build-vignettes .

RUN R CMD INSTALL --no-docs --no-multiarch --no-staged-install OmicNavigator_*.tar.gz

RUN Rscript -e 'OmicNavigator::installApp()'

RUN Rscript scripts/install-test-study.R
