# rplot_ggplot.R - plotting functions for the rplot command
# Yu-Ru Lin
# date: May 12, 2012

# TODO: implement all graphs in http://wiki.stdout.org/rcookbook/Graphs/

# initialize ggplot with blank them instead of default gray background
rplot_init <- function(liblist=c()) {
  suppressPackageStartupMessages( require(ggplot2,quietly=T) )  
  theme_update(panel.background=theme_blank(), 
         panel.grid.major=theme_line(colour = "grey90"),
  			 panel.border=theme_blank())
  if ('MASS' %in% liblist) suppressPackageStartupMessages( require(MASS,quietly=T) )  
  if ('gplots' %in% liblist) suppressPackageStartupMessages( require(gplots,quietly=T) )  
}

# take S random samples from the data and generat ranked indices for the top K samples
# (-k|--rank) (s|sample)<S>(r|rank)<K>
# S = K = length(data) if not sepcified 
# if (s|sample) is not specified, output the whole data
# if (r|rank) is not specified, output unordered data
get_rank_samples_old <- function(X,opt) {
  X$index = 1:nrow(X)
  if ( grepl('(s|sample)\\d*', opt$rank) ) {
    if (opt$verbose) printf('sampling ')
    n <- length(X$x)
    if ( grepl('(s|sample)\\d+', opt$rank) ) {
      ns <- as.numeric(gsub('(.*)(s|sample)(\\d+)(\\w*)', '\\3', opt$rank))
      if (!is.na(ns)) n <- ns
    }
    if (opt$verbose) printf('%d points\n',n)
    idx <- sample(X$index)[1:n]
    X <- X[idx,]
    X$index = 1:n
  }  
  if ( grepl('(r|rank)\\d*', opt$rank) ) {
    if (opt$verbose) printf('ranking ')
    n <- length(X$x)
    if ( grepl('(r|rank)\\d+', opt$rank) ) {
      ns <- as.numeric(gsub('(.*)(r|rank)(\\d+)(\\w*)', '\\3', opt$rank))
      if (!is.na(ns)) n <- ns
    }
    if (opt$verbose) printf('%d points\n',n)
    idx <- order(X$x,decreasing=F)[1:n]
    X <- X[idx,]
    X$index = 1:n
  }
  X
}
get_rank_samples <- function(X,opt) {
  X$index = 1:nrow(X)
  X
}
# plot with points and/or lines
rplot_with <- function(p,opt) {
  if (opt$scatterplot) {
    if ( grepl('(p|point)', opt$with) ) p <- p + geom_point()
    if ( grepl('(l|line)', opt$with) ) p <- p + geom_line()    
  } else if (opt$bin!='') {
    if ( grepl('(p|pdf|density)', opt$bin) ) {      
      if ( grepl('(p|point)', opt$with) ) p <- p + stat_bin(aes(y=..density..),geom='point')
      if ( grepl('(l|line)', opt$with) ) p <- p + stat_bin(aes(y=..density..),geom='line')
    } else { # grepl('(c|count|raw)\\d*', opt$bin)
      if ( grepl('(p|point)', opt$with) ) p <- p + stat_bin(aes(y=..count..),geom='point')
      if ( grepl('(l|line)', opt$with) ) p <- p + stat_bin(aes(y=..count..),geom='line')
    }
  }
  p
}

# plot with log-scale on xy
rplot_log <- function(p,opt) {
  has.label = F
  if (has.str(opt$log,'x')) {
    if ( grepl('(lnX|xe|xn)', opt$log, ignore.case=T) ) p <- p + scale_x_log(name=opt$xlab)
    if ( grepl('(log2X|lg2X|l2x|x2)', opt$log, ignore.case=T) ) p <- p + scale_x_log2(name=opt$xlab)
    else p <- p + scale_x_log10(name=opt$xlab) # (log10X|lg10X|l10x|lx|x10)
    has.label = T
  }
  if (has.str(opt$log,'y')) {
    if ( grepl('(lnY|ye|yn)', opt$log, ignore.case=T) ) p <- p + scale_y_log(name=opt$ylab)
    if ( grepl('(log2Y|lg2Y|l2y|y2)', opt$log, ignore.case=T) ) p <- p + scale_y_log2(name=opt$ylab)
    else p <- p + scale_y_log10(name=opt$ylab) # (log10Y|lg10Y|l10y|ly|y10)
    has.label = T
  }
  if (!has.label) p <- rplot_label(p,opt)
  p
}

rplot_label <- function(p,opt) {
  p <- p + scale_x_continuous(name=opt$xlab)
  p <- p + scale_y_continuous(name=opt$ylab)    
  p
}

# output plot
rplot_output <- function(p,opt) {
  if (!is.null(opt$title)) p <- p + opts(title=opt$title)
  png(opt$tmpimg)
  print(p)
  dev.off()
  system(sprintf('%s %s',opt$opencmd,opt$tmpimg))  
}

rplot_with <- function(p,opt) {
  if ( grepl('(p|point)', opt$with) ) p <- p + geom_point()
  else if ( grepl('(o|circle)', opt$with) ) p <- p + geom_point(shape=1)
  else if ( grepl('(sq|square)', opt$with) ) p <- p + geom_point(shape=0)
  else if ( grepl('(tri|triangle)', opt$with) ) p <- p + geom_point(shape=2)
  if ( grepl('(l|line)', opt$with) ) p <- p + geom_line()   
  else if ( grepl('dash', opt$with) ) p <- p + geom_line(linetype='dashed')   
  else if ( grepl('dot', opt$with) ) p <- p + geom_line(linetype='dotted')   
  p
}  

rplot_scatterplot <- function(X,opt) {
  if (opt$verbose) printf('scatterplot\n')
  rplot_init()
  X <- as.data.frame(X)
  if (ncol(X)==1) { # (i,x) indexing plot
    X <- get_rank_samples(X,opt)
    p <- ggplot(X, aes(index, x))    
  } else { # (x,y) scatterplot
    p <- ggplot(X, aes(x, y)) 
  }
  p <- rplot_with(p,opt)
  p <- rplot_log(p,opt)
  rplot_output(p,opt)
}

rplot_histogram <- function(X,opt) {
  # the histogram plot uses linear binning, even with log-scale xy
  # TODO: use log-binning
  # TODO: set bin size
  # @see: http://wiki.stdout.org/rcookbook/Graphs/Plotting%20distributions%20(ggplot2)/
  if (opt$verbose) printf('histogram/density plot\n')
  rplot_with <- function(p,opt) {
    if ( grepl('(pdf|density)', opt$bin) ) { 
#       p <- p + geom_density()
      p <- p + geom_histogram(aes(y=..density..), # Histogram with density instead of count on y-axis 
                          colour="black", fill="white") +
            geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
#       if ( grepl('(p|point)', opt$with) ) p <- p + stat_bin(aes(y=..density..),geom='point')
#       if ( grepl('(l|line)', opt$with) ) p <- p + stat_bin(aes(y=..density..),geom='line')
    } else { # by default, disply count on y-axis
      # grepl('(c|count|raw)\\d*', opt$bin) 
      if ( !grepl('(p|point|l|line)', opt$with) ) p <- p + geom_histogram(aes(y=..count..), colour="black", fill="white")

      if ( grepl('(p|point)', opt$with) ) p <- p + stat_bin(aes(y=..count..),geom='point')
      if ( grepl('(l|line)', opt$with) ) p <- p + stat_bin(aes(y=..count..),geom='line')
    }
    if ( grepl('(m|mean)', opt$with) ) {
      p <- p + geom_vline(aes(xintercept=mean(x, na.rm=T)), # Ignore NA values for mean 
                        color="red", linetype="dashed", size=1)
    }
    p
  }
  get_Xs <- function(X,opt) {
    DF <- NULL
    for (j in 1:ncol(X)) {
      Y <- data.frame(x=X[,j])
      Y <- get_rank_samples(Y,opt)
      df <- data.frame(x=Y$x,var=j)
      DF <- rbind(DF,df)
    }
    DF$var <- factor(DF$var)
    DF
  }
  
  rplot_init()
  X <- as.data.frame(X)
  if (ncol(X)==1) {
    X <- get_rank_samples(X,opt)
    p <- ggplot(X, aes(x)) 
    p <- rplot_with(p,opt)
  } else {
    X <- get_Xs(X, opt) # collapse multiple variables into one with addition 'var' variable  
    if ( grepl('(pdf|density)', opt$bin) ) {
      if ( grepl('(g|grp|group)', opt$bin) ) p <- ggplot(X, aes(x, y=..density.., colour=var, fill=var ) )
      else if ( grepl('facet', opt$bin) ) p <- ggplot(X, aes(x, y=..density..) ) + facet_grid(var ~ ., scales = "free")
      else p <- ggplot(X, aes(x, y=..density..) )
      p <- p + geom_density(alpha=0.5)
    } else { # count
      if ( grepl('(g|grp|group)', opt$bin) ) p <- ggplot(X, aes(x, y=..count.., colour=var, fill=var ) )
      else if ( grepl('facet', opt$bin) ) p <- ggplot(X, aes(x, y=..count..) ) + facet_grid(var ~ ., scales = "free")
      else p <- ggplot(X, aes(x, y=..count..) )  
      p <- p + geom_histogram(alpha=0.5, colour="black", fill="white", position="identity")
    }
  }
  p <- rplot_log(p,opt)
  rplot_output(p,opt)
}

rplot_cdf <- function(X,opt) {
  if (opt$verbose) printf('CDF plot\n')
  get_Ys <- function(X, opt) {
    DF <- NULL
    for (j in 1:ncol(X)) {
      Y <- data.frame(x=X[,j])
      Y <- get_rank_samples(Y,opt)
      x <- Y$x; u <- unique(x); l <- 1
      if ( grepl('(c|count)',opt$cdf) ) l <- length(x)
      df <- data.frame(x=u,Fn=ecdf(x)(u)*l,var=j )
      DF <- rbind(DF,df)
    }
    DF$var <- factor(DF$var)
    DF    
  }
  rplot_init()
  X <- as.data.frame(X)
  if (ncol(X)==1) {
    X <- get_rank_samples(X,opt)
    x <- X$x; u <- unique(x); l <- 1
    if ( grepl('(c|count)',opt$cdf) ) l <- length(x)
    Y <- data.frame(x=u,Fn=ecdf(x)(u)*l )
    p <- ggplot(Y, aes(x,y=Fn)) 
  } else {
    Y <- get_Ys(X, opt) # collapse multiple variables into one with addition 'var' variable  
    if ( grepl('facet', opt$cdf) ) p <- ggplot(Y, aes(x, y=Fn) ) + facet_grid(var ~ ., scales = "free")
    else p <- ggplot(Y, aes(x, y=Fn, colour=var) ) # grepl('(g|grp|group)', opt$cdf)
    
  }  
  p <- rplot_with(p,opt)
  p <- rplot_log(p,opt)
  rplot_output(p,opt)
}

# source('http://addictedtor.free.fr/graphiques/sources/source_1.R')
# modified the code from the above url
kde2dplot <- function(d,                # a 2d density computed by kde2D
                      ncol=50,          # the number of colors to use
#                       zlim=c(0,max(z)), # limits in z coordinates
                      nlevels=20,       # see option nlevels in contour
                      theta=30,         # see option theta in persp
                      phi=30,           # see option phi in persp
                      xlab='x',ylab='y',zlab='density')           
{
  # TODO: fix the xlab,ylab in contour plot
  z <- d$z; zlim=c(0,max(z))
  nrz <- nrow(z)
  ncz <- ncol(z)
  
  couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol)
  fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1]
  dim(fcol) <- c(nrz,ncz)
  fcol      <- fcol[-nrz,-ncz]
  
  par(mfrow=c(1,2),mar=c(0.5,0.5,0.5,0.5))
#   if (is.finite(zlim[2])) 
  persp(d,col=fcol,zlim=zlim,theta=theta,phi=phi,zlab=zlab,xlab=xlab,ylab=ylab)
  
  par(mar=c(2,2,2,2))
  image(d,col=couleurs)
  contour(d,add=T,nlevels=nlevels,xlab=xlab,ylab=ylab)
  box()
}

rplot_density2d <- function(X,opt) {
  if (opt$verbose) printf('2D density plot\n')
  rplot_init(liblist=c('MASS'))
  X <- as.data.frame(X)
  x <- X$x; y <- X$y
  xlab <- 'x'; ylab <- 'y'
  if (!is.null(opt$xlab)) xlab <- opt$xlab
  if (!is.null(opt$ylab)) ylab <- opt$ylab
  if (opt$log!='') {
    n0 = nrow(X)
    X <- subset(X,x>0 & y>0)
    n = nrow(X)
    printf('remove %d/%d (%.2f) before log-transformation\n',n0-n,n0,(n0-n)/n0)
    x <- X$x; y <- X$y
    if (has.str(opt$log,'x')) {
      if ( grepl('(lnX|xe|xn)', opt$log, ignore.case=T) ) {x <- log(x); xlab <- 'ln(x)'}
      if ( grepl('(log2X|lg2X|l2x|x2)', opt$log, ignore.case=T) ) {x <- log2(x); xlab <- 'log2(x)'}
      else {x <- log10(x); xlab <- 'log10(x)'} # (log10X|lg10X|l10x|lx|x10)
    }
    if (has.str(opt$log,'y')) {
      if ( grepl('(lnY|ye|yn)', opt$log, ignore.case=T) ) {y <- log(y); ylab <- 'ln(y)'}
      if ( grepl('(log2Y|lg2Y|l2y|y2)', opt$log, ignore.case=T) ) {y <- log2(y); ylab <- 'log2(y)'}
      else {y <- log10(y); ylab <- 'log10(y)'} # (log10Y|lg10Y|l10y|ly|y10)
    }
  }
  f <- kde2d(x,y)

  png(opt$tmpimg,width=800,height=400)
  kde2dplot(f,xlab=xlab,ylab=ylab)
  dev.off()
  system(sprintf('%s %s',opt$opencmd,opt$tmpimg))  
}

rplot_hist2d <- function(X,opt) {
  if (opt$verbose) printf('2D histogram plot\n')
  rplot_init(liblist=c('gplots'))
  X <- as.data.frame(X)
  x <- X$x; y <- X$y
  xlab <- 'x'; ylab <- 'y'

  if (opt$log!='') {
    n0 = nrow(X)
    X <- subset(X,x>0 & y>0)
    n = nrow(X)
    printf('remove %d/%d (%.2f) before log-transformation\n',n0-n,n0,(n0-n)/n0)
    x <- X$x; y <- X$y
    if (has.str(opt$log,'x')) {
      if ( grepl('(lnX|xe|xn)', opt$log, ignore.case=T) ) {x <- log(x); xlab <- 'ln(x)'}
      if ( grepl('(log2X|lg2X|l2x|x2)', opt$log, ignore.case=T) ) {x <- log2(x); xlab <- 'log2(x)'}
      else {x <- log10(x); xlab <- 'log10(x)'} # (log10X|lg10X|l10x|lx|x10)
    }
    if (has.str(opt$log,'y')) {
      if ( grepl('(lnY|ye|yn)', opt$log, ignore.case=T) ) {y <- log(y); ylab <- 'ln(y)'}
      if ( grepl('(log2Y|lg2Y|l2y|y2)', opt$log, ignore.case=T) ) {y <- log2(y); ylab <- 'log2(y)'}
      else {y <- log10(y); ylab <- 'log10(y)'} # (log10Y|lg10Y|l10y|ly|y10)
    }
  }
  
  png(opt$tmpimg)
  hist2d(x,y, nbins=10, col = c("white",heat.colors(16)), xlab=xlab, ylab=ylab)
  rug(x,side=1)
  rug(y,side=2)  
  dev.off()
  system(sprintf('%s %s',opt$opencmd,opt$tmpimg))  
}

rplot_trend <- function(X,opt) {
  rplot_init()
  X <- as.data.frame(X)
  if (ncol(X)<2) {
    printf('Error: need more than 2 variables to plot the y-against-x trend line.\n')
    q(status=1)
  }
  xbreaks <- quantile(X$x, probs = seq(0,1,1.0/opt$bins),na.rm=T,names=F)
  Y <- X; Y$grp <- 1:nrow(X)
  for (i in 1:length(xbreaks)-1) Y$grp[which(Y$x >= xbreaks[i] & Y$x < xbreaks[i+1])] <- 0.5*(xbreaks[i]+xbreaks[i+1])
  Y$x <- factor(Y$grp)
  Y <- summarySE(Y, measurevar="y", groupvars=c('x'))
  Y$x <- xbreaks[as.numeric(Y$x)]
  # Standard error of the mean
  p <- ggplot(Y, aes(x, y)) + 
    geom_errorbar(aes(ymin=y-se, ymax=y+se), width=.1) +
    geom_line() +
    geom_point()  
  #p <- rplot_with(p,opt)
  p <- rplot_log(p,opt)
  rplot_output(p,opt)
 
}