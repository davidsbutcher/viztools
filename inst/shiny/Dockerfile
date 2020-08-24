# DOCKERFILE for viztools
# github.com/davidsbutcher/viztools

# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# install R packages from CRAN 

#RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('assertthat', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('glue', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('purrr', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('stringr', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('forcats', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('markdown', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('tibble', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('UpSetR', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('fs', repos='http://cran.rstudio.com/')"
#RUN R -e "install.packages('tidyr', repos='http://cran.rstudio.com/')"

# install R packages from github

RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('davidsbutcher/viztools')"
RUN R -e "devtools::install_github('hrbrmstr/waffle')"

# copy the app to the image
COPY app.R /var/viztools/
COPY about.md /var/viztools/
COPY /www/ /var/viztools/www/

# select port
EXPOSE 3838

# run app
CMD R -e 'shiny::runApp("/var/viztools", port = 3838, host = "0.0.0.0")'