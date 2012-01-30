# ez

The aim of the `ez` package for R is to provide a simplified/unified interface to common analysis techniques, including analysis of variance and mixed effects modeling. 

This site not only hosts the ongoing code development for `ez`, but also serves as the forum (https://github.com/mike-lawrence/ez/issues) to report bugs and request features associated `ez`.

A discussion forum can be found at http://groups.google.com/group/ez4r

## Note:

To automatically download and install the very latest ez code any time you run a given R script, install the "devtools" library (`install.packages('devtools')`) then put the following at the top of your script:
    library(devtools)
    install_github('ggplot2','hadley')
    install_github('ez','mike-lawrence')   

The above will change to
    library(devtools)
    install_github('ggplot2','hadley')
    install_github('ez','mike-lawrence')   

as soon as ggplot2 v0.9.0 is available on CRAN (March 1, 2012?)

