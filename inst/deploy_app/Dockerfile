# Get the rocker image, latest shiny from rocker in this case:
# Base image https://hub.docker.com/u/rocker/
# For latest version use: FROM rocker/shiny:latest
FROM rocker/shiny-verse:4.3

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Install package remotes [to allow installGithub.r for github packages]:
RUN install2.r data.table remotes devtools rlang rmapshaper sf sp plyr here \
    methods assertthat leaflet leaflet.extras leafem leafpop mapview DT waiter \
    webshot satellite servr svglite shinydashboard shinyWidgets shinyjs

# Create a working directory in the image:
WORKDIR /home/shinyusr

# Copy the folder "app" (containing the shiny app) to the working directory ".":
COPY inst/app app

# Copy the deployment script from the deployment folder to the working dircetory:
COPY inst/deploy_app/deploy.R deploy.R

# Set the command to run once the container runs:
CMD Rscript deploy.R

### Notes:
## To build this file with the name "test1":
# 1. make sure this file is on project root
# 2. navigate to R's local terminal and execute:
# docker build -t test1 -f deploy-shiny/DockerfileDev .
# 3. if the docker file was in root directory:
# docker build -t test1 .
# 4. if the build exists, this can be overriden:
# docker build --no-cache -t test1 -f deploy-shiny/DockerfileDev .

## To run the image from the same terminal:
# 1. if the imaage require environment vars and stored in ".Renviron"
# docker run --env-file .Renviron test1

## In github actions (if test1 was the image name):
# docker build -t test1 -f deploy-shiny/DockerfileDev .
