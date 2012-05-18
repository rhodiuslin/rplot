rplot
=====
    rplot is a command-line tool that generates basic statistical plot or summary of a given data file. It calls R functions through RScript. This tool is inspired by the handy [datatools] created by [bagrow].

Examples
========
    $ rplot -h                     # print usage information
    $ rplot -f x.dat -s            # print summary
    $ rplot -f x.dat -p            # scatterplot
    $ rplot -f xy.dat -p -l xy     # 2D scatterplot with log-scale on xy-axes
    $ rplot -f x.dat -b            # historam plot
    $ rplot -f xy.dat -b grouppdf  # show multiple density plot into groups
    $ rplot -f xy.dat -b facetpdf  # show multiple density plot into facets
    $ rplot -f xy.dat -x 1 -c -s   # extract the first column to generate CDF plot and summary
    $ rplot -f xy.dat -r           # print correlation matrix
    $ rplot -f karate.gml -s -p         # print the network summary and plot the network
    $ rplot -f karate.edgelist -p deg   # plot the network degree distribution
    $ rplot -f x.dat -p -k s5000 -w pl  # sample 5000 points from the data and plot with point+line
    $ rplot -f xy.dat -p trend -l x2    # show trend of y against log2(x)
See test.sh for more examples.

Install
=======
Get the git repository:
    $ git clone git://github.com/rhodiuslin/rplot.git
Add the `rplot` directory to your path.  For example, put the following code in your `bashrc`.
    if [ -d "$HOME/rplot" ]; then
        export PATH=~/rplot:$PATH
    fi
In the `rplot` directory, add a soft link:
    $ ln -s rplot.R rplot 

Requirements
============
* bash and awk
* [R][] 2.1x (The following libraries will be automatically installed the first time you run `rplot`, if they are missing.)
  * [getopt][]
  * [ggplot2][]
  * [igraph]
  * [MASS]
    
[R]: http://www.r-project.org/
[getopt]: http://cran.r-project.org/web/packages/getopt/index.html
[ggplot2]: http://had.co.nz/ggplot2/
[igraph]: http://igraph.sourceforge.net/
[MASS]: http://cran.r-project.org/web/packages/MASS/index.html
[datatools]: https://github.com/bagrow/datatools
[bagrow]: https://github.com/bagrow