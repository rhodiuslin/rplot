#!/bin/bash
# test rplot functions

function pause(){
   read -p "$*"
}

pause 'Print help. Press [Enter] key to continue...'
rplot -h
pause 'Show summary. Press [Enter] key to continue...'
rplot -f test/x.dat -n -t 3 -s
rplot -f test/xy.dat -n 3 -t 3 -s -v
rplot -f test/xy.dat -r
pause 'Show scatterplot. Press [Enter] key to continue...'
rplot -f test/x.dat -p --xlab x-index
rplot -f test/x.dat -p -l x --xlab 'log(x-index)' --title test-title
rplot -f test/x.dat -p -k s500r -w pl
rplot -f test/x.dat -p -k s500r -w odash
rplot -f test/x.dat -p -k r -l xy -v
rplot -f test/xy.dat -x 1 -p
rplot -f test/xy.dat -p
pause 'Show histogram/density plot. Press [Enter] key to continue...'
rplot -f test/xy.dat -x 2 -b
rplot -f test/xy.dat -x 2 -b pdf
rplot -f test/xy.dat -b grouppdf
rplot -f test/xy.dat -b facetpdf
pause 'Show CDF plot. Press [Enter] key to continue...'
rplot -f test/xy.dat -x 1 -c -s -v
rplot -f test/xy.dat -c
rplot -f test/xy.dat -c facet -w l
pause 'Show 2D density plot. Press [Enter] key to continue...'
rplot -f test/xy.dat -e
pause 'Show trend y against x. Press [Enter] key to continue...'
rplot -f test/x.dat -p trend 
rplot -f test/xy.dat -p trend 
rplot -f test/xy.dat -p trend -l x
pause 'Show network. Press [Enter] key to continue...'
rplot -f test/karate.edgelist -n 3 -s
rplot -f test/karate.gml -n 6 -s
rplot -f test/karate.gml -s
rplot -f test/karate.gml -s -p -v
rplot -f test/karate.edgelist -s -p
rplot -f test/karate.gml -p deg
rplot -f test/karate.edgelist -p deg
