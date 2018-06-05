# chelyabinsk

R package for geochemical datasets

This refers [gem package --
casteml](https://github.com/misasa/casteml "follow instruction") and [r package --
MedusaRClient](https://github.com/misasa/MedusaRClient "follow instruction").  See
manual at
`http://dream.misasa.okayama-u.ac.jp/documentation/chelyabinsk-manual.pdf'.

# Dependency

## [GNU R](https://www.r-project.org/ "follow instruction")
## [gem package -- casteml](https://github.com/misasa/casteml "follow instruction")
## [r package -- MedusaRClient](https://github.com/misasa/MedusaRClient "follow instruction")

# User's guide

To install this package issue following command.

    R> install.packages('devtools')
    R> devtools::install_github('misasa/chelyabinsk')
    R> devtools::install_github('misasa/MedusaRClient')
    R> library('chelyabinsk')
    R> cbk.path("periodic-dflame1.csv")

When you see message such like below, try following.

    Error in curl::curl_fetch_disk(url, x$path, handle = handle) :   Problem with the SSL CA cert (path? access rights?

    R> install.packages(c("curl", "httr"))

Examples to plot analyses related to a stone and its descendants are
shown below. Issue following commands.

    $ Rscript -e "chelyabinsk::cbk.plot(\"20130528105235-594267\", category=\"trace\")"

    R> library(chelyabinsk)
    R> pmlame <- cbk.read.casteml("20130528105235-594267")
    R> cbk.plot.spider(pmlame,property="atomicnumber",reference="Wasson.1988")

Or write a Rscript with following lines.

    #!/usr/bin/env Rscript
    library(chelyabinsk)
    rplotfile <- 'Rplots.pdf'; pdf(rplotfile)
    cbk.plot("20130528105235-594267")
    dev.off()

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

5. Update manual

Copy manual automatically created by roxygen to
http://dream.misasa.okayama-u.ac.jp/documentation/chelyabinsk-manual.pdf
when necessary.

```
$ cp ./chelyabinsk.Rcheck/chelyabinsk-manual.pdf ~/devel-godigo/documentation/
```


6. Push to the server or build/install locally

```
$ R CMD build ../chelyabinsk
$ R CMD install chelyabinsk_1.0.tar.gz
```

# Developer's TODO

1. Update R scripts in https://github.com/misasa/casteml/tree/master/template/plot
2. Write document in texinfo
3. Convert inst/extdata/periodic-table.csv to more handy format
4. Suppress Warnings on check

# Developer's TODO (done)

1. Write document in each function by Roxygen2
2. Write test code
3. Write document in R/chelyabinsk-package.R
4. Have inst/extdata/ref.csv in casteml-dataframe format ref1.dataframe
5. Revise cbk.ref.R accodringly or create new function
