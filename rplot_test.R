rplot_test <- function() {
  require('getopt',quietly=T)
  
  #get options, using the spec as defined by the enclosed list.
  #we read the options from the default: commandArgs(TRUE).
  opt = getopt(c(
    'verbose', 'v', 2, "integer",
    'help', 'h', 0, "logical",
    'count', 'c', 1, "integer",
    'mean', 'm', 1, "double",
    'sd', 's', 1, "double"
  ));
  
  #help was asked for.
  if ( !is.null(opt$help) ) {
    #get the script name (only works when invoked with Rscript).
    #self = commandArgs()[1];
    self = '$ rplot'
    #print a friendly message and exit with a non-zero error code
    cat(paste("Usage: ",self," [-[vh]] [-[-mean|m] <mean>] [-[-sd|s] <sd>] [-[-count|c] <count>]\n",sep=""));
    q(status=1);
  }
  
  #set some reasonable defaults for the options that are needed,
  #but were not specified.
  if ( is.null(opt$mean ) ) { opt$mean = 0 }
  if ( is.null(opt$sd ) ) { opt$sd = 1 }
  if ( is.null(opt$count ) ) { opt$count = 10 }
  if ( is.null(opt$verbose ) ) { opt$verbose = FALSE }
  #print some progress messages to stderr, if requested.
  if ( opt$verbose ) { write("writing...",stderr()); }
  
  #do some operation based on user input.
  cat(paste(rnorm(opt$count,mean=opt$mean,sd=opt$sd),collapse="\n"));
  cat("\n");
  
  #signal success and exit.
  q(status=0);
  
}
