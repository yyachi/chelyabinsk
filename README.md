# chelyabinsk
R package for geochemical data plot

# User's guide

To install and use this package issue following command.

    R> library(devtools)                    % provide install_github
    R> install_github('misasa/chelyabinsk') % instead of install
    
    R> library('chelyabinsk')

# Developer's guide

1. Clone the project from GitHub.

  https://github.com/misasa/chelyabinsk

2. Locate your script with a function to directory "R".  Write document with rule of Roxygen2.

3. Generate documentation by following commands.

```
$ cd ~/devel-godigo/R/chelyabinsk
$ Rscript -e "devtools::document()"
```

4. Write and run test

```
$ vi ./tests/testthat/test-mytest.R
$ R CMD check ../chelyabinsk
```

5. Push to the server
