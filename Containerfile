FROM rocker/shiny:latest

RUN R -e "install.packages(c('dplyr', 'ggplot2', 'ggrepel', 'markdown', 'lubridate', 'scales', 'shinyWidgets', 'tibble'), repos='https://cloud.r-project.org/')"

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

COPY ./shiny_app/app.R ./app.R