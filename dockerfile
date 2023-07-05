FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libssl-dev \
    libgit2-dev \
    libxml2-dev \
    libudunits2-dev \
    cmake \
    libgmp-dev \
    libmpc-dev \
    libglpk-dev 

# install R packages required
# UI side libraries (change it dependeing on the packages you need)
RUN R -e "install.packages(c('devtools','remotes','ggplot2','tidyverse','knitr'), dependencies=TRUE)"
RUN R -e "install.packages(c('bs4Dash','networkD3','rhandsontable','formattable'), dependencies=TRUE)"
RUN R -e "install.packages(c('shinyWidgets','shinycssloaders','htmltools','plotly'), dependencies=TRUE)"

# Server side libraries
RUN R -e "install.packages(c('symengine','n1qn1','PreciseSums'), dependencies=TRUE)"
RUN R -e "install.packages('rxode2', dependencies=TRUE)"
RUN R -e "install.packages('nlmixr2', dependencies=TRUE)"
RUN R -e "install.packages('shinyjs', dependencies=TRUE)"

# Data
RUN R -e "install.packages(c('reactable','kableExtra','data.table','furrr'), dependencies=TRUE)"
# networkD3 package addon
RUN R -e "install.packages('igraph', dependencies=TRUE)"

# copy the app to the image
COPY . /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh

#COPY /drug /srv/shiny-server/drug
#COPY /base /srv/shiny-server/base
#COPY /temp /srv/shiny-server/temp
#COPY app.R /srv/shiny-server
#COPY 1.mods.R /srv/shiny-server
#COPY 2.des.R /srv/shiny-server
#COPY 3.plot.R /srv/shiny-server
#COPY 4.dm.R /srv/shiny-server
#COPY 5.sim.R /srv/shiny-server

# select port
EXPOSE 3838

# print logs
ENV SHINY_LOG_STDERR=1
RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
# permission
#RUN sed -i 's/run_as shiny;/run_as root;/g' /etc/shiny-server/shiny-server.conf
#RUN chown -R root:root /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server/temp

# run app
CMD ["/usr/bin/shiny-server.sh"]