FROM rocker/tidyverse:3.4.4

RUN mkdir /agora-digital
WORKDIR /agora-digital
COPY DESCRIPTION .
RUN Rscript -e 'devtools::install_deps()'
RUN Rscript -e 'devtools::install_github("analytics-ufcg/rcongresso")'
COPY . .
RUN Rscript -e 'devtools::install()'
