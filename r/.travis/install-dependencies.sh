#!/bin/sh

# Install R dependencies

echo 'R install'
(
   sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys E084DAB9
   echo 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/' | sudo tee -a /etc/apt/sources.list
   sudo apt-get update && sudo apt-get install r-base r-base-dev -y
   sudo Rscript -e "install.packages('optparse', repos='http://cran.rstudio.org')"
   sudo Rscript -e "install.packages('seqinr', repos='http://cran.rstudio.org')"
   sudo Rscript -e "install.packages('logging', repos='http://cran.rstudio.org')"
   cd r
   sudo R CMD INSTALL biotool
)
