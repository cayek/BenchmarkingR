library(BenchmarkingR)
context("bench test")

test_that("Create a simple benchmarkingR project from scratch", {

  bench.proj = bench( dir.name = "~/PatatorHomeDir/Projects/BenchmarkingR/", new=TRUE )

  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(x) { return(x) },
                               method.name="func1",
                               method.description="")

  expect_error( bench.addmethod(bench.proj,
                                method.func=function(x) { return(x) },
                                method.name="func1",
                                method.description=""), "A method already exists with this name" )

  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data1",
                                data.description="")

  expect_error( bench.adddataset(bench.proj,
                                 data.G=diag(2),
                                 data.X=c(1,2),
                                 data.name="data1",
                                 data.outlier = c(2),
                                 data.description=""), "A data set already exists with this name" )


  bench.save(bench.proj)

})


test_that("Create a simple benchmarkingR project from scratch", {

  bench.proj = bench( dir.name = "~/PatatorHomeDir/Projects/BenchmarkingR/", new=TRUE )

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
                               method.func=function(G,X) { return(1:ncol(G)) },
                               method.name="func1",
                               method.description="")

  # run over all method and dataset
  bench.proj = bench.run(bench.proj, data.name, method.name)

  expect_warning(bench.run(bench.proj, data.name, method.name),"Done: use argument again=TRUE to run it again.")

  bench.proj = bench.remove(bench.proj, data.name, method.name)

  expect_equal(nrow(bench.proj$summary),0)
  expect_equal(nrow(bench.proj$pvalue),0)
  expect_equal(nrow(bench.proj$fdr_power),0)


  # add other method and data set
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data2",
                                data.description="")
  bench.proj = bench.addmethod(bench.proj,
                               method.func=function(G,X) { return(1:ncol(G)) },
                               method.name="func2",
                               method.description="")

  # try to run over all data set dan function
  bench.proj = bench.run(bench.proj)

  expect_warning(bench.run(bench.proj, data.name, method.name),"Done: use argument again=TRUE to run it again.")

  bench.proj = bench.run(bench.proj,again = TRUE)


})


test_that("Test consistency between env and directory.", {

  #   dirbench.name = "~/PatatorHomeDir/Projects/BenchmarkingR/"
  #   # remove the directory
  #   unlink( dirbench.name, recursive = TRUE )
  #   dir.create(dirbench.name)
  #
  #   bench.proj = bench( dir.name = dirbench.name, new=TRUE )


})
