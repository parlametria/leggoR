FROM rocker/tidyverse:3.6.1

WORKDIR /agora-digital

#Prepare environment and copy files
RUN apt-get update
RUN apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev vim less -y
COPY DESCRIPTION .
RUN Rscript -e 'update.packages(checkBuilt=TRUE, ask=FALSE)'
RUN Rscript -e 'install.packages("devtools"); devtools::install_deps()'
COPY . .

#Install rcongresso from local branch
RUN Rscript -e 'devtools::install("rcongresso/")'

#Remove rcongresso files to avoid including in leggoR package
RUN rm -rf rcongresso

#Install leggoR package
RUN Rscript -e 'devtools::install()'
