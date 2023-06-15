FROM rocker/r-ver:4

MAINTAINER Julien Barde "julien.barde@ird.fr"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libjpeg-dev \
    default-jre \
    default-jdk \
    libxml2-dev \
    libudunits2-dev \
    libproj-dev \
    libgeos-dev \
    libgdal-dev \
    git \
    texlive-latex-base \
    texlive-fonts-recommended \
    texlive-formats-extra \
    libv8-dev \
	  libsodium-dev \
    libsecret-1-dev \
    libnlopt-dev \
    libharfbuzz-dev \
    libfribidi-dev

RUN apt-get update && apt-get upgrade -y
RUN apt-get update && apt-get -y install cmake

# install core package dependencies
RUN install2.r --error --skipinstalled --ncpus -1 remotes
RUN R -e "install.packages(c('jsonlite','yaml'), repos='https://cran.r-project.org/')"
# clone app



RUN git -C /root/ clone https://github.com/juldebar/dockerization_shiny.git && echo "OK!"
RUN ln -s /root/smt-shiny /srv/dockerization_shiny
# install R app package dependencies
RUN R -e "source('./srv/dockerization_shiny/install.R')"

EXPOSE 3838

ENV SMT_LOG=session.log

RUN apt-get -y update
RUN apt-get install -y curl
#Development
CMD ["R", "-e shiny::runApp('/srv/dockerization_shiny',port=3838,host='0.0.0.0')"]
