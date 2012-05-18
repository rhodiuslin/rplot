# rplot_igraph.R - network plotting functions for the rplot command
# Yu-Ru Lin
# date: May 16, 2012

graph_init <- function(opt) {
  suppressPackageStartupMessages( require(igraph,quietly=T) )  
}

load_graph <- function(opt) {
  if (opt$verbose) printf('loading graph file %s...\n',opt$file)
  graph_init()
  ext <- gsub("(.*)\\.([^([:space:]|.)]+)$",'\\2',opt$file)
  g = read.graph(opt$file,format=ext)
  list(X=NULL,opt=opt,g=g)
}

show_graph_summary <- function(g,opt) {
  summary(g)
}

graph_plot <- function(g,opt) {
  if (opt$verbose) printf('plot graph\n')
  png(opt$tmpimg)
  plot(g, layout=layout.fruchterman.reingold, vertex.size=4, vertex.label.dist=0.7, vertex.color="#277EBB", vertex.frame.color="#075AA7", vertex.label.family="Helvetica", vertex.label.color="#444444", vertex.label.cex=0.75)  
  dev.off()
  system(sprintf('%s %s',opt$opencmd,opt$tmpimg))  
}

graph_plot_degree_distribution <- function(g,opt) {
  if (opt$verbose) printf('plot degree distribution\n')
  png(opt$tmpimg)
  plot(degree.distribution(g, cumulative=T), pch=20,xlab="degree", ylab="cumulative frequency")
  dev.off()
  system(sprintf('%s %s',opt$opencmd,opt$tmpimg))  
}