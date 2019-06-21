stone <- "20160627191317"
url <- paste0('https://dream.misasa.okayama-u.ac.jp/demo/records/',stone,'.pml')
GET_stub <- stubthat::stub(GET)

test_that("without directAuth", {
  GET_stub$strictlyExpects(url = url, config = authenticate('admin','admin',type='basic'), handle = NULL)
  with_mock(
    yaml.load_file = function(...){list(uri = 'https://dream.misasa.okayama-u.ac.jp/demo/', user = 'admin', password = 'admin')},
    `httr::GET` = GET_stub$f,
    `httr::content` = function(...){},
    `httr::writeBin` = function(...){},   
    expect_error(cbk.download.casteml.1(stone), NA) 
  )
})

test_that("with directAuth", {
  directAuth <- list(uri="https://dream.misasa.okayama-u.ac.jp/demo/",user='admin',password='admin')
  GET_stub$strictlyExpects(url = url, config = authenticate('admin','admin',type='basic'), handle = NULL)
  with_mock(
    `httr::GET` = GET_stub$f,
    `httr::content` = function(...){},
    `httr::writeBin` = function(...){},   
    expect_error(cbk.download.casteml.1(stone, directAuth = directAuth), NA) 
  )
})
