FROM r-base:4.0.5

RUN echo 'APT::Get::Install-Recommends "false";' >> /etc/apt/apt.conf
RUN echo 'options(Ncpus=4, repos=structure(c(CRAN="https://cloud.r-project.org")))' > ~/.Rprofile
RUN echo 'installOrQuit <- function(p) {tryCatch(install.packages(p), warning=function(e){q(status=1)})}' >> ~/.Rprofile

# external dependencies
RUN apt-get update && apt-get install -y pandoc pandoc-citeproc imagemagick && apt-get clean

# prefer binary R packages, if they are available
RUN apt-get update && apt-get install -y \
r-cran-tidyverse r-cran-data.table \
r-cran-patchwork r-cran-cowplot \
r-cran-magic r-cran-magick r-cran-ggforce \
r-cran-rmarkdown r-cran-bookdown

# add missing source packages in blocks of 4
RUN Rscript -e "installOrQuit(c('here', 'pacman', 'assertr', 'viridis', 'lemon'))"

