library(BenchmarkingR)
context("bench remove test")


test_that("Remove method", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create methode and dataset
  function.test = function(G,X,parameter) {
    return(list(pvalue = (1:ncol(G))/ncol(G),K = parameter$K))
  }
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(3),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(3),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data2",
                                data.description="")
  bench.proj = bench.run(bench.proj)


  # try to remove method
  bench.proj = bench.removemethod(bench.proj, "func2")
  # methods
  expect_equal(nrow(bench.proj$methods()),0)
  # summary
  expect_equal(nrow(bench.proj$summary()),0)
  # parameters
  expect_equal(nrow(bench.proj$parameters()),0)
  # results
  expect_equal(nrow(bench.proj$results()),0)
  # pvalue
  expect_equal(nrow(bench.proj$pvalues()),0)

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("Remove dataset", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # create methode and dataset
  function.test = function(G,X,parameter) {
    return(list(pvalue = (1:ncol(G))/ncol(G),K = parameter$K))
  }
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(3),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(3),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data2",
                                data.description="")
  bench.proj = bench.run(bench.proj)


  # try to remove method
  bench.proj = bench.removedataset(bench.proj, "data1")
  # methods
  expect_equal(as.character(bench.proj$dataset()$name),"data2")
  # summary
  expect_equal(as.character(bench.proj$summary()$data),"data2")
  # parameters
  expect_equal(as.character(bench.proj$parameters()$data),"data2")
  # results
  expect_equal(as.character(bench.proj$results()$data),"data2")
  # fdr_power
  # TODO
  # pvalue
  expect_equal(as.character(bench.proj$pvalues()$data),c("data2","data2","data2"))

  unlink( bench.proj$dirbench, recursive = TRUE )

})




