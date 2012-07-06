# rplot_main.R - the primary functions for rplot command
# Yu-Ru Lin
# date: May 12, 2012

usage <- function(spath) cat(readLines(sprintf('%s/readme',spath)),sep='\n')

rplot_main <- function(spath='') {
  # TODO: need a better way to handle the arguments
  #get options, using the spec as defined by the enclosed list.
  #we read the options from the default: commandArgs(TRUE).
  opt = getopt(c(
    'verbose', 'v', 2, "integer",
    'help', 'h', 0, "logical",
    'opencmd', 'o', 1, "character",
    'file', 'f', 2, "character",
    'delim', 'd', 1, "character",
    'head', 'n', 2, "integer",
    'tail', 't', 2, "integer",
    'summary', 's', 2, "character",
    'corr', 'r', 0, "logical",
    'plot', 'p', 2, "character",
    'scatterplot', '0', 0, "logical",
    'bin', 'b', 2, "character",
    'cdf', 'c', 2, "character",
    'density2d', 'e', 2, "character",
    'with', 'w', 1, "character",
    'log', 'l', 2, "character",
    'rank', 'k', 2, "character",
    'extract','x', 1, "character",
    'xlab','1',1,'character',
    'ylab','2',1,'character',
    'title','3',1,'character',
    'test','4',0,'logical'
  ));
#   print(opt)
  opt$spath <- spath
  #help was asked for.
  if ( !is.null(opt$help) ) {
    #get the script name (only works when invoked with Rscript).
    #self = commandArgs()[1];
    usage(opt$spath)
    q(status=1);
  }
  if (!is.null(opt$test)) {
    system(sprintf('./test.sh'))
  }

  opt$tmpimg <- 'tmp.png'
  opt$tmpdat <- 'tmp.dat'
  
  #set some reasonable defaults for the options that are needed,
  #but were not specified.
  if ( is.null(opt$file ) ) {
    if(isatty(stdin())==T) { cat('error: rplot requires stdin or an input file.\n'); q(status=1) }
    write.table(read.table(file("stdin"), header=F),opt$tmpdat, row.names=F,col.names=F);
    opt$file <- opt$tmpdat
  }
  if ( is.null(opt$verbose ) ) { opt$verbose = FALSE }
  if ( is.null(opt$delim ) ) { opt$delim = guess_delim(opt) }
  if ( is.null(opt$opencmd ) ) { 
    sysname = Sys.info()['sysname']
    if (sysname=='Darwin') opt$opencmd = '/usr/bin/open'
    else if (sysname=='Linux') opt$opencmd = '/usr/bin/gnome-open'
    else if (has.str(sysname,'windows')) opt$opencmd = 'start' # not tested!
    if (opt$verbose) printf('set command to open output in %s: %s\n',opt$opencmd,sysname)
  }
  if ( is.null(opt$summary ) ) { opt$summary = '' }
  if ( is.null(opt$corr ) ) { opt$corr = F }
  if ( is.null(opt$scatterplot) && is.null(opt$plot) ) { opt$plot = ''; opt$scatterplot = F }
  if ( is.null(opt$with ) ) { 
    opt$with = 'p' 
    if ( !is.null(opt$bin ) ) opt$with = 'h' # the default for histogram plot is 'bar' instead of 'point'
  }
  if ( is.null(opt$log ) ) { opt$log = '' }
  if ( is.null(opt$rank ) ) { opt$rank = 'r' }
  if ( is.null(opt$bin ) ) { opt$bin = '' }
  if ( is.null(opt$cdf ) ) { opt$cdf = '' }
  if ( is.null(opt$density2d ) ) { opt$density2d = '' }
  if ( is.null(opt$extract ) ) { opt$extract = '' }

  opt$bins <- 20
  
  #print some progress messages to stderr, if requested.
#   if ( opt$verbose ) { write("writing...",stderr()); }
#   print(opt)
  #do some operation based on user input.
  if (!is.null(opt$head) || !is.null(opt$tail)) show_file_snippet_fast(opt) 
  else if (opt$summary!=''
      || opt$corr
      || opt$bin!=''
      || opt$cdf!=''
      || opt$density2d!=''
      || opt$plot!=''
      || opt$scatterplot) {
    xo <- load_file(opt)
    
    if (!is.null(xo$X)) {
      if (opt$summary!='') show_file_summary(xo$X,xo$opt)
      if (opt$corr) show_correlation(xo$X,xo$opt)
      if (opt$bin!='') rplot_histogram(xo$X,xo$opt) # plot histogram or pdf
      else if (opt$cdf!='') rplot_cdf(xo$X,xo$opt) # plot cdf
      else if (opt$density2d=='hist') rplot_hist2d(xo$X,xo$opt) # plot 2D density (persp + contour)
      else if (opt$density2d!='') rplot_density2d(xo$X,xo$opt) # plot 2D density (persp + contour)
      else if (opt$plot=='trend') rplot_trend(xo$X,xo$opt)
      else if (opt$plot!='' || opt$scatterplot) rplot_scatterplot(xo$X,xo$opt) # scatterplot is the default plotting function    
    } 
    else if (!is.null(xo$g)) {
      if (opt$summary!='') show_graph_summary(xo$g,xo$opt)
      if (opt$plot=='deg') graph_plot_degree_distribution(xo$g,xo$opt) # scatterplot is the default plotting function    
      else if (opt$plot!='') graph_plot(xo$g,xo$opt) # scatterplot is the default plotting function    
      
    }
    
    
  }
  
  cat("\n");
  
  #signal success and exit.
  q(status=0);
  
}
