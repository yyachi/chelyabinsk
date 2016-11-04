# chelyabinsk
R package for geochemical datasets.  This is referred by [gem package -- casteml](https://github.com/misasa/casteml "follow instruction").

# Dependency

## [GNU R](https://www.r-project.org/ "follow instruction")

# User's guide

To install this package issue following command.

    R> install.packages('devtools')

    R> library(devtools); install_github('misasa/chelyabinsk')

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

1. Write document in each function by Roxygen2
2. Write test code
3. Update R scripts in https://github.com/misasa/casteml/tree/master/template/plot
4. Write document in texinfo
5. Convert inst/extdata/periodic-table.csv to more handy format
6. Suppress Warnings on check

# Developer's TODO (done)
1. Write document in R/chelyabinsk-package.R
2. Have inst/extdata/ref.csv in casteml-dataframe format ref1.dataframe
3. Revise cbk.ref.R accodringly or create new function

