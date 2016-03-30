library(BenchmarkingR)
context("data viewer")



test_that("pca computation", {

  dir.name = tempdir()
  dir.create(dir.name)
  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create data set
  n = 10
  L = 100
  X = matrix(rnorm(n), nrow = n, ncol = 1)
  G = matrix(rbinom(n*L, 1, 0.5), nrow = n, ncol = L)
  outlier = sample(1:L, 30)

  # add data set
  bench.adddataset(bench.proj, G, X, outlier, "data1", "descr" )

  # get data.exploration
  data.exploration = bench.getdata.exploration(bench.proj, "data1")
  expect_is(data.exploration,"list")

  # test warning
  expect_warning(bench.pca(bench.proj, "data2"))

  # pca
  data.exploration = bench.pca(bench.proj, "data1", again = FALSE)

  unlink( bench.proj$dirbench, recursive = TRUE )

})



test_that("snmf computation", {

  dir.name = tempdir()
  dir.create(dir.name)
  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create data set
  n = 10
  L = 100
  X = matrix(rnorm(n), nrow = n, ncol = 1)
  G = matrix(rbinom(n*L, 1, 0.5), nrow = n, ncol = L)
  outlier = sample(1:L, 30)

  # add data set
  bench.adddataset(bench.proj, G, X, outlier, "data1", "descr" )

  # get data.exploration
  data.exploration = bench.getdata.exploration(bench.proj, "data1")
  expect_is(data.exploration,"list")

  # test warning
  expect_warning(bench.snmf(bench.proj, "data2"))

  # snmf
  data.exploration = bench.snmf(bench.proj, "data1", 2, again = FALSE)

  # test if every tmp file was removed
  files = list.files(bench.proj$dirbench)
  expect_equal(length(files),4)

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("dataviewer", {

  dir.name = tempdir()
  dir.create(dir.name)
  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create data set
  n = 10
  L = 100
  X = matrix(rnorm(n), nrow = n, ncol = 1)
  G = matrix(rbinom(n*L, 1, 0.5), nrow = n, ncol = L)
  outlier = sample(1:L, 30)

  # add data set
  bench.adddataset(bench.proj, G, X, outlier, "data1", "simple 1" )
  bench.adddataset(bench.proj, G, X, outlier, "data2", "simple 2" )

  t=bench.snmf(bench.proj,"data1",K = 2)
  t=bench.pca(bench.proj,"data1")

  # run shiny app for view dataset
  #bench.dataviewer(bench.proj)

  unlink( bench.proj$dirbench, recursive = TRUE )

})



test_that("resultviewer", {

  dir.name = tempdir()
  dir.create(dir.name)
  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create data set
  n = 10
  L = 100
  X = matrix(rnorm(n), nrow = n, ncol = 1)
  G = matrix(rbinom(n*L, 1, 0.5), nrow = n, ncol = L)
  outlier = sample(1:L, 30)

  # add data set
  bench.adddataset(bench.proj, G, X, outlier, "data1", "simple 1" )
  bench.adddataset(bench.proj, G, X, outlier, "data2", "simple 2" )

  # add methods
  function.test = function(G,X,parameter) {
    return(list(pvalue = (1:ncol(G))/ncol(G), B = parameter$K*(1:ncol(G)),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func1",
                               method.description="")
  # run
  bench.run(bench.proj, parameter = list(K = 1))

  #bench.plotres(bench.proj, data.name = "data1", method.name = "func1")

  # run shiny app for view dataset
  #bench.resultviewer(bench.proj)

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("comparaisonviewer", {

  dir.name = tempdir()
  dir.create(dir.name)
  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create data set
  n = 10
  L = 100
  X = matrix(rnorm(n), nrow = n, ncol = 1)
  G = matrix(rbinom(n*L, 1, 0.5), nrow = n, ncol = L)
  outlier = sample(1:L, 30)

  # add data set
  bench.adddataset(bench.proj, G, X, outlier, "data1", "simple 1" )
  bench.adddataset(bench.proj, G, X, outlier, "data2", "simple 2" )

  # add methods
  function.test = function(G,X,parameter) {
    return(list(pvalue = (1:ncol(G))/ncol(G), B = parameter$K*(1:ncol(G)),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func1",
                               method.description="")
  # run
  bench.run(bench.proj, parameter = list(K = 1))

  #bench.plotres(bench.proj, data.name = "data1", method.name = "func1")

  # run shiny app for view dataset
  bench.comparaisonviewer(bench.proj)

  unlink( bench.proj$dirbench, recursive = TRUE )

})



