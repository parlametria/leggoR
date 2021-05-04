FROM rocker/tidyverse:3.6.2

WORKDIR /agora-digital

#Prepare environment and copy files
RUN apt-get update > /dev/null
RUN apt-get install libssl-dev libxml2-dev libcurl4-openssl-dev libgit2-dev vim less git -y > /dev/null
RUN apt-get update > /dev/null
RUN apt-get install -y libjpeg-dev libpoppler-cpp-dev > /dev/null
ENV TZ=America/Sao_Paulo
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
COPY DESCRIPTION .
RUN Rscript -e 'update.packages(checkBuilt=TRUE, ask=FALSE, quiet=TRUE)' > /dev/null
RUN Rscript -e 'install.packages(c("devtools", "digest"), quiet=TRUE); devtools::install_deps(quiet=TRUE)' > /dev/null
RUN Rscript -e 'devtools::install_version("dplyr", version = "1.0.3", repos = "http://cran.us.r-project.org", quiet=TRUE)' > /dev/null
RUN Rscript -e 'devtools::install_version("testthat", version = "3.0.1", repos = "http://cran.us.r-project.org", quiet=TRUE)' > /dev/null
RUN Rscript -e 'install.packages(c("futile.logger", "pscl", "pdftools", "eeptools"), quiet=TRUE)' > /dev/null
COPY . .

#Install rcongresso from local branch
ARG clone_rcongresso=true
RUN if [ "$clone_rcongresso" = "false" ] ;  then Rscript -e 'devtools::install("rcongresso/", quiet=TRUE)' > /dev/null; else Rscript -e 'devtools::install_github("analytics-ufcg/rcongresso", quiet=TRUE)' > /dev/null; fi


#Remove rcongresso files to avoid including in leggoR package
RUN rm -rf rcongresso > /dev/null

#Install leggoR package
RUN Rscript -e 'devtools::install(upgrade = "never", quiet=TRUE)' > /dev/null

#Install perfilparlamentar package
RUN Rscript -e 'devtools::install_github("parlametria/perfil-parlamentarR@main", quiet=TRUE)' > /dev/null
