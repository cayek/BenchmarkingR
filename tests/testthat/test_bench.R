library(BenchmarkingR)
context("bench test")

test_that("Create a simple benchmarkingR project from scratch", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(x) { return(x) },
                               method.name="func1",
                               method.description="")

  expect_warning( bench.addmethod(bench.proj,
                                method.func=function(x) { return(x) },
                                method.name="func1",
                                method.description=""), "A method already exists with this name" )

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  expect_warning( bench.adddataset(bench.proj,
                                 data.G=diag(2),
                                 data.X=c(1,2),
                                 data.name="data1",
                                 data.outlier = c(2),
                                 data.description=""), "A data set already exists with this name" )


  bench.save(bench.proj)

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("Create a simple benchmarkingR project from scratch", {

  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  data.name = "data1"
  method.name = "func1"


  expect_error( bench.run(bench.proj, data.name, method.name ), "The data set data1 does not exist.")

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  expect_error( bench.run(bench.proj, data.name, method.name ), "The method func1 does not exist.")


  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(G,X,parameter) { return(list(pvalue = 1:ncol(G))) },
                               method.name="func1",
                               method.description="")

  # run over all method and dataset
  bench.proj = bench.run(bench.proj, data.name, method.name)

  expect_warning(bench.run(bench.proj, data.name, method.name),"Done: use argument again=TRUE to run it again.")

  bench.proj = bench.remove(bench.proj, data.name, method.name)

  expect_equal(nrow(bench.proj$summary()),0)
  expect_equal(nrow(bench.proj$pvalue()),0)

  # add other method and data set
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data2",
                                data.description="")
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(G,X,parameter) { return(list(pvalue = 1:ncol(G))) },
                               method.name="func2",
                               method.description="")

  # try to run over all data set dan function
  bench.proj = bench.run(bench.proj)

  expect_warning(bench.run(bench.proj, data.name, method.name),"Done: use argument again=TRUE to run it again.")

  bench.proj = bench.run(bench.proj,again = TRUE)


  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("Test consistency between env and directory.", {
  dir.name = tempdir()
  dir.create(dir.name)
  dirbench.name = file.path(dir.name,"BenchmarkingR_project")
  # remove the directory
  unlink( dirbench.name, recursive = TRUE )
  dir.create(dirbench.name)

  expect_error( bench( dir.name = dir.name, new=TRUE ) )

  unlink( dirbench.name, recursive = TRUE )
})



test_that("run computation with parameter", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(G,X) { return(list(pvalue = 1:ncol(G))) },
                               method.name="func1",
                               method.description="")

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")


  expect_warning(bench.addparameter(bench.proj, data.name = "data21", method.name = "func1", parameter=list(K=1)),
                 "Any data set exists with this name")

  expect_warning(bench.addparameter(bench.proj, data.name = "data1", method.name = "func41", parameter=list(K=1)),
                 "Any method exists with this name")

  bench.proj = bench.addparameter(bench.proj, data.name = "data1", method.name = "func1", parameter=list(K=1))
  expect_equal(nrow(bench.proj$parameters()),1)
  expect_warning(bench.getparam(bench.proj, data.name = "data1", method.name = "func2"),"No result availables for this run")
  param = bench.getparam(bench.proj, data.name = "data1", method.name = "func1")
  expect_equal(param$K,1)
  bench.proj = bench.addparameter(bench.proj, data.name = "data1", method.name = "func1", parameter=list(K=2))
  param = bench.getparam(bench.proj, data.name = "data1", method.name = "func1")
  expect_equal(param$K,2)
  expect_equal(nrow(bench.proj$parameters()),1)
  # try to run algo on dataset with different parameter


  # test result
  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")

  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2", K = 1, B= 2)

  param = bench.getparam(bench.proj, data.name = "data1", method.name = "func2")

  expect_is(param,"list")
  expect_equal(param$B,2)
  expect_equal(param$K,1)

  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func21")
  expect_null(result$K)
  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func2")

  expect_equal(result$K,1)

  # with K = 2
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2", K = 2, B= 2)
  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func2")
  expect_equal(result$K,2)

  unlink( bench.proj$dirbench, recursive = TRUE )
})


test_that("Test if on run with parameter", {

  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  # run without parameter
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2")
  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func2")
  expect_null(result$K)

  # run with parameter K = 1
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2", K=1)
  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func2")
  expect_equal(result$K,1)

  # run without parameter,so it supposes to use K = 1, Actually no, it replace parameter !!
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2")
  result = bench.getresult(bench.proj, data.name = "data1", method.name = "func2")
  expect_null(result$K)

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("Test summary parameter", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  # run without parameter
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2")
  expect_equal(as.character(bench.proj$summary()$parameter),"Parameters :")

  # run with parameter
  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2",K=1,B=2)
  expect_equal(as.character(bench.proj$summary()$parameter),"Parameters : K = 1 | B = 2 |")

  unlink( bench.proj$dirbench, recursive = TRUE )


})


test_that("Test time of run", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  function.test = function(G,X,parameter) {
    a=rnorm(10000000)
    return(list(pvalue = 1:ncol(G),K = parameter$K))
  }

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  bench.proj = bench.run(bench.proj, data.name = "data1",again=TRUE, method.name = "func2")
  expect_more_than(bench.proj$summary()$time,0.0)

  unlink( bench.proj$dirbench, recursive = TRUE )


})


test_that("replace method or dataset", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # replace method
  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = parameter$K))
  }
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function.test,
                               method.name="func2",
                               method.description="")
  method = bench.getmethod(bench.proj,"func2")
  expect_equal(method(diag(2),1,list(K=3))$K,3)
  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = 1))
  }
  expect_warning(bench.addmethod(bench.proj,
                                 method.func=function.test,
                                 method.name="func2",
                                 method.description="test", replace = TRUE),
                 "You are using replace=TRUE, only the function will be replaced. To replace the description use bench.removemethod before")
  expect_equal(as.character(bench.proj$methods()$description),"")
  method = bench.getmethod(bench.proj,"func2")
  expect_equal(method(diag(2),1,list(K=3))$K,1)

  # replace database
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(3),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")
  data = bench.getdata(bench.proj,"data1")
  expect_equal(nrow(data$G),3)
  expect_warning(bench.adddataset(bench.proj,
                                  data.G=diag(2),
                                  data.X=c(1,2),
                                  data.outlier = c(2),
                                  data.name="data1",
                                  data.description="test",replace = TRUE),
                 "You are using replace=TRUE, only the function will be replaced. To replace the description use bench.removedataset before")
  expect_equal(as.character(bench.proj$dataset()$description),"")
  data = bench.getdata(bench.proj,"data1")
  expect_equal(nrow(data$G),2)

  unlink( bench.proj$dirbench, recursive = TRUE )


})



