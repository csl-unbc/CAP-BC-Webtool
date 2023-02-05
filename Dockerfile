# base image
FROM xaviercll/wheretowork-base:4.2.2

## install app
RUN mkdir /app
COPY inst /app/inst
COPY man /app/man
COPY R /app/R
COPY .Rbuildignore /app
COPY DESCRIPTION /app
COPY NAMESPACE /app
RUN cd /app && \
    Rscript -e 'remotes::install_local(upgrade = "never")'

## prepare data cap-bc project
COPY Makefile /app
RUN cd /app && make cap-bc

## set user
USER shiny

## select port
EXPOSE 3838

## configure shiny
## store environmental variables
ENV R_SHINY_PORT=3838
ENV R_SHINY_HOST=0.0.0.0
RUN env | grep R_SHINY_PORT > /home/shiny/.Renviron && \
    env | grep R_SHINY_HOST >> /home/shiny/.Renviron

## set working directory
RUN mkdir /home/shiny/app
COPY app.R /home/shiny/app/app.R
WORKDIR /home/shiny/app

## run app
CMD ["/usr/local/bin/Rscript", "/home/shiny/app/app.R"]
