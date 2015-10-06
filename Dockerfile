FROM r-base
RUN apt-get -y update
RUN apt-get -y install libxml2-dev
RUN apt-get -y install libssl-dev 
RUN apt-get -y install libcurl4-openssl-dev

RUN Rscript -e 'install.packages("devtools")'

RUN Rscript -e "install.packages('devtools')"
ADD ./tmisc /tmp/tmisc
RUN Rscript -e "require('devtools');install_deps('/tmp/tmisc')"
RUN R CMD INSTALL /tmp/tmisc

CMD R --no-save