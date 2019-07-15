FROM rocker/tidyverse:3.4.4

RUN mkdir /agora-digital
WORKDIR /agora-digital
COPY . .
RUN apt-get update
RUN apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev -y
RUN Rscript -e 'install.packages(c("checkpoint","devtools"))'
RUN Rscript -e 'checkpoint::setSnapshot("2019-07-09"); devtools::install_deps(); devtools::install_version("dplyr", version = "0.8.3")'
RUN Rscript -e 'devtools::install_github("analytics-ufcg/rcongresso")'
RUN Rscript -e 'devtools::install()'
