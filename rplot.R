#! /usr/bin/Rscript
##--vanilla --default-packages=utils,getopt

# rplot.R - a script that generate quick plot or summary for a given data file.
#           Use 'rplot -h' to see the usage information. 
#           Curretnly only test on Linux and Mac.
# Yu-Ru Lin
# date: May 12, 2012

# set a prior desired mirror to avoid evoking Tcl/Tk interface
options(repos='http://r.iq.harvard.edu') # USA
# options(verbose=F,echo=F) 
liblist = c('utils','getopt','ggplot2','igraph','MASS')

spath <- dirname(Sys.which('rplot')) # get the path of current script
source(sprintf('%s/rplot_utils.R',spath))

# check required libraries; install non-installed libraries for the first time
for (lib in liblist) {
#   cat('checking package',lib,'\n')
  if (!is.installed(lib)) {
    cat('installing package',lib,'\n')
    res <- try(install.packages(lib))
    if (inherits(res, "try-error")) q(status=1) else q()
  }
}
require(getopt,quietly=T)

# source(sprintf('%s/rplot_test.R',spath))
source(sprintf('%s/rplot_stat.R',spath))
source(sprintf('%s/rplot_ggplot.R',spath))
source(sprintf('%s/rplot_igraph.R',spath))
source(sprintf('%s/rplot_main.R',spath))

# args <- commandArgs(TRUE); print(args)
suppressWarnings(rplot_main())
