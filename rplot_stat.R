# rplot_stat.R - summary functions for the rplot command
# Yu-Ru Lin
# date: May 12, 2012

get_line_count <- function(opt) {
  l = unlist( strsplit(readLines(pipe(sprintf('wc -l %s',opt$file))),split=' ') )
  nlines = l[grep('\\d+',l,perl=T,value=F)]
  n <- as.numeric(nlines)      
  n
}
guess_delim <- function(opt) {
  # TODO: stupid guess, need to improve..
  lines <- readLines(opt$file,n=5)
  line <- lines[5]
  if (has.str(line,',')) delim = ','
  else if (has.str(line,'\t')) delim = '\t'
  else delim = ' '
  if (opt$verbose) printf('file delimiter: %s\n',delim)
  delim
}
guess_header <- function(opt) {
  line <- readLines(opt$file,n=1)  
  ns <- as.numeric(gsub('([:space:]*(\\d+\\.\\d+))(.*)', '\\1', line))
  header = T
  if (!is.na(ns)) header = F
  header
}
load_file <-function(opt) {
  #TODO: detect the header, delimitor
  #TODO: multiple tuples (x1,y1,..)(x2,y2,..)
  if ( grepl('\\.(gml|edgelist|graphml|pajek)$',opt$file) ) return(load_graph(opt))
  if (opt$verbose) printf('loading...\n')
  set_default_colnames <- function(X) {
    cnames <- colnames(X)
    if (cnames[1]=='V1') {
      defaults = c('x','y','z','w')
      m <- ncol(X)
      return(defaults[1:m])
    }
    return(cnames)
  }
  extract_fields <- function(opt) {
    printf('extract field %s into %s\n',opt$extract,opt$tmpdat)
    c <- sprintf('cut -d"%s" -f %s %s> %s',opt$delim,opt$extract,opt$file,opt$tmpdat)
    system(c)
    opt$file <- opt$tmpdat    
    opt
  }
  sample_sort_file <- function(opt) {
    # NOTE: this avoid to call GNU sort and shuf (or sort -R) 
    # GNU coreutils may not be included on Mac or other *nix system
    # (although it's easy on Mac, e.g. brew install coreutils)
    if ( grepl('(s|sample)\\d*', opt$rank) ) {
      if (opt$verbose) printf('sampling\n')
      n <- get_line_count(opt)
      if ( grepl('(s|sample)\\d+', opt$rank) ) {
        ns <- as.numeric(gsub('(.*)(s|sample)(\\d+)(\\w*)', '\\3', opt$rank))
        if (!is.na(ns)) n <- ns
      }
      if (opt$verbose) printf('\t%d points\n',n)
#       c = sprintf('shuf -n %d %s > %s',n,opt$file,opt$tmpdat)
      awkshuf = sprintf("awk 'BEGIN {srand()} {print int(rand()*1000000000),\",\",$0}' | sort -n | cut -d\",\" -f 2-")
      c = sprintf("cat %s | %s | sed 's/^ *//' | head -n %d > tmp; mv tmp %s",opt$file, awkshuf, n, opt$tmpdat)
      # the sed command removes leading white space; maybe there is a faster way for this?
      system(c)
      opt$file <- opt$tmpdat
    }  
    if ( grepl('(r|rank)\\d*', opt$rank) ) {
      if (opt$verbose) printf('ranking\n')
      n <- get_line_count(opt)
      if ( grepl('(r|rank)\\d+', opt$rank) ) {
        ns <- as.numeric(gsub('(.*)(r|rank)(\\d+)(\\w*)', '\\3', opt$rank))
        if (!is.na(ns)) n <- ns
      }
      if (opt$verbose) printf('\t%d points\n',n)
      c = sprintf('cat %s | sort -g | head -n %d > tmp; mv tmp %s',opt$file,n,opt$tmpdat,opt$tmpdat)
      system(c)
      opt$file <- opt$tmpdat
    }    
    opt
  }
  if (opt$extract!='') opt <- extract_fields(opt)
  if (opt$rank!='') opt <- sample_sort_file(opt)
  X <- read.csv(opt$file, sep=opt$delim, header=guess_header(opt),strip.white=T)
  colnames(X) <- set_default_colnames(X)
  list(X=X,opt=opt)
}

show_file_snippet_fast <- function(opt) {
  system(sprintf('wc -l %s',opt$file)) # count the lines in the file; fast but cannot customized file information
  system(sprintf('head -%d %s',opt$head, opt$file))
  cat('...','\n')
  system(sprintf('tail -%d %s',opt$tail, opt$file))
}

show_file_snippet <- function(opt) {
  n <- get_line_count(opt)
  printf('file snippet: %s (%d lines)\n',opt$file, n)
  cat(readLines(opt$file,n=opt$head),sep='\n')
  cat('...','\n')
  cat(readLines(pipe(sprintf('tail -%d %s',opt$tail, opt$file))),sep='\n')
}

show_file_summary <- function(X,opt) {
  printf('file summary: %s (%s lines)\n',opt$file, nrow(X))
  print(summary(X))
  if ( grepl('(m|more)',opt$summary) ) {
    cnames = colnames(X)
    rnames = rownames(X)
    for (j in 1:ncol(X)) {
      x <- X[,j]
      printf('%4s: [%8.4f (%6s), %8.4f (%6s)] sigma:%8.4f\n',cnames[j],
             min(x),rnames[which.min(X[,j])],max(x),rnames[which.max(X[,j])],sd(x) )
    }
  }
}

show_correlation <- function(X,opt) {
  print(corstars(X))
}
