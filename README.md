# chelyabinsk
R package for geochemical datasets

# User's guide

install this package issue following command.

    R> install.packages('devtools')

    R> library(devtools)                    % provide install_github
    R> install_github('misasa/chelyabinsk') % instead of install

    R> library('chelyabinsk')
    R> cbk.path("periodic-table.csv")

# Developer's guide

1. Clone the project from GitHub and install appropriate packages.

  https://github.com/misasa/chelyabinsk

```
$ cd ~/devel-godigo
$ git clone https://github.com/misasa/chelyabinsk.git
R> install.packages('testthat')
R> install.packages("roxygen2")
```

2. Locate your script with a function to directory "R".  Write
   document with rule of Roxygen2.  Do not use |#' | in main code
   besides the documentation in Roxygen2.

3. Generate Rdoc by following commands.

```
$ cd ~/devel-godigo/chelyabinsk
$ Rscript -e "devtools::document()"
```

```
R> setwd(path.expand("~/devel-godigo/chelyabinsk"))
R> devtools::document()
```

4. Run test

```
$ vi ./tests/testthat/test-mytest.R
$ R CMD check ../chelyabinsk
```

5. Push to the server or build/install locally

```
$ R CMD build ../chelyabinsk
$ R CMD install chelyabinsk_1.0.tar.gz
```

# Developer's TODO

1. Write document in R/chelyabinsk-package.R
2. Write document in each function by Roxygen2
3. Write test code
4. Update R scripts in https://github.com/misasa/casteml/tree/master/template/plot
5. Write document in texinfo
6. Have inst/extdata/ref.csv in casteml-dataframe format
7. Revise cbk.ref.R accodringly or create new function
8. Convert inst/extdata/periodic-table.csv to more handy format
