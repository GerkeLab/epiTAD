FROM rocker/shiny:3.5.1
LABEL maintainer="Travis Gerke (Travis.Gerke@moffitt.org)"

# Install system dependencies for required packages
RUN apt-get update && apt-get install -y libssl-dev libxml2-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/ \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "install.packages(c('BiocManager', 'shinydashboard', 'data.table', 'shinycssloaders', 'jsonlite', 'colorspace', 'DT'))" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN Rscript -e "BiocManager::install(c('haploR', 'HiTC', 'Sushi', 'biomaRt'))" \
  && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

RUN rm -r /srv/shiny-server/*
ARG SHINY_APP_IDLE_TIMEOUT=60
RUN sed -i "s/directory_index on;/app_idle_timeout ${SHINY_APP_IDLE_TIMEOUT};/g" /etc/shiny-server/shiny-server.conf
COPY . /srv/shiny-server/epiTAD
