FROM rocker/r2u:jammy

RUN apt-get update

COPY . /app

WORKDIR /app

RUN bash scripts/install-dependencies-r2u.sh

RUN R CMD build --no-manual --no-build-vignettes .

RUN R CMD INSTALL --no-docs --no-multiarch --no-staged-install OmicNavigator_*.tar.gz

RUN Rscript -e 'OmicNavigator::installApp()'

RUN Rscript scripts/install-test-study.R

EXPOSE 5656

CMD ["/bin/bash"]

# To use:
#
# build: docker build -t omicnavigator .
#
# start interactive session: docker run --rm -it -p 5656:5656 omicnavigator
#
# Run the app: Rscript -e 'OmicNavigator::startApp()'
#
# Open in browser: http://localhost:5656/ocpu/library/OmicNavigator/
