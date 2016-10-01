# chelyabinsk
R package for geochemical data plot

# User's guide

To install and use this package issue following command.

    R> install.packages('devtools')
    R> install.packages('testthat')

    R> library(devtools)                    % provide install_github
    R> install_github('misasa/chelyabinsk') % instead of install

    R> library('chelyabinsk')
    $> castbl.path("periodic-table.csv")

# Developer's guide

1. Clone the project from GitHub.

  https://github.com/misasa/chelyabinsk

```
$ cd ~/devel-godigo
$ git clone https://github.com/misasa/chelyabinsk.git
```

2. Locate your script with a function to directory "R".  Write
   document with rule of Roxygen2.

3. Generate documentation by following commands.

```
$ cd ~/devel-godigo/chelyabinsk
$ Rscript -e "devtools::document()"
```

```
R> devtools::document()
```

4. Write and run test

```
$ vi ./tests/testthat/test-mytest.R
$ R CMD check ../chelyabinsk
```

5. Push to the server
