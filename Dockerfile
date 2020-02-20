FROM rocker/tidyverse:3.6.1

RUN mkdir /agora-digital
WORKDIR /agora-digital
RUN apt-get update
RUN apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev -y
COPY DESCRIPTION .
RUN Rscript -e 'update.packages(checkBuilt=TRUE, ask=FALSE)'
RUN Rscript -e 'install.packages("devtools"); devtools::install_deps()'
RUN Rscript -e 'devtools::install_github("analytics-ufcg/rcongresso")'
COPY . .
RUN Rscript -e 'devtools::install()'
