FROM rocker/tidyverse:4.1.0

ENV miniconda3_version="py39_4.9.2"
ENV miniconda_bin_dir="/opt/miniconda/bin"
ENV PATH="${PATH}:${miniconda_bin_dir}"

RUN apt-get update -qq -y \
    && apt-get install --no-install-recommends -qq -y \
        bash-completion \
        curl \
        gosu \
        libxml2-dev \
        zlib1g-dev \
        libxtst6 \
        libxt6 \
    && apt-get -y autoclean \
    && apt-get -y autoremove \
    && rm -rf /var/lib/apt/lists/* \

    # Install miniconda
    && curl -fsSLO https://repo.anaconda.com/miniconda/Miniconda3-${miniconda3_version}-Linux-x86_64.sh \
    && bash Miniconda3-${miniconda3_version}-Linux-x86_64.sh \
        -b \
        -p /opt/miniconda \
    && rm -f Miniconda3-${miniconda3_version}-Linux-x86_64.sh \
    && useradd -u 1500 -s /bin/bash miniconda \
    && chown -R miniconda:miniconda /opt/miniconda \
    && chmod -R go-w /opt/miniconda \
    && conda --version

COPY ./conda /tmp/conda
RUN conda init bash \
    && conda env create -f /tmp/conda/environment.yml \
    && rm -fr /tmp/conda \
    && cp /usr/lib/x86_64-linux-gnu/libssl.so.1.1 \
        /opt/miniconda/envs/sage-bionetworks/lib/libssl.so.1.1 \
    && conda activate base || true \
    && echo "conda activate sage-bionetworks" >> ~/.bashrc


RUN R -e "library(devtools);devtools::install_version('reticulate', version = '1.19', repos='http://cran.rstudio.com/')"

COPY create_rds_files.R /usr/local/bin/
RUN chmod a+x /usr/local/bin/create_rds_files.R
CMD Rscript /usr/local/bin/create_rds_files.R