FROM rocker/r-ver:3.6.0

RUN apt-get update && apt-get install -y \
        sudo \
        pandoc \
        python3 \
        python3-pip \
        libssl-dev \
        libffi-dev 
        
# Add app to docker
RUN mkdir /root/projectLive/
ADD . /root/projectLive/
WORKDIR /root/projectLive/

RUN echo "options(repos = c(CRAN = 'https://cloud.r-project.org/', SAGE = 'http://ran.synapse.org'), download.file.method = 'libcurl')" 

# Restore from renv
RUN R -e 'renv::restore()'

EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');projectLive::run_app()"