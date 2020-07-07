# wfs
Extract and reshape WFS data using R

This is work in progress developing an R package for extracting 
and reshaping World Fertility Survey (WFS) data from the Demographic 
and Health Surveys (DHS) data archive.
The WFS was a large demographic data collection effort that conducted 
over forty surveys between 1972 and 1984. Its successor, the DHS 
program, has made available a large number of public-use data files, see
[wfs.dhsprogram.com](http://wfs.dhsprogram.com) for more information.

Install the development version directly from GitHub by first installing
Hadley Wickman's `devtools` package, and then calling the function
`install_github("grodri/nuptfer")`. 

There is a [wiki](https://github.com/grodri/wfs/wiki) with comments on 
the development process.
The code for viewing dictionaries, extracting subsets of variables,
and making local copies of the dictionary and data files is working
but still subject to change. 
The code for reshaping the union and birth histories is undergoing
a major revision and shoul dnot be used at this time.

