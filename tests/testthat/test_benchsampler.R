library(BenchmarkingR)
context("bench sampler test")


test_that("add/remove/run sampler", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # try to add sampler
  sampler = function(parameter) {
    n = parameter$n
    L = parameter$L
    return(list(G=matrix(sample(n*L),n,L), X = matrix(sample(n),n,1), outlier = c(1)))
  }
  bench.addsampler(bench.proj, sampler, "sampler1", "desc sampler1")
  expect_equal(nrow(bench.proj$sampler()),1)


  # try to run a sampler
  expect_warning(bench.runsampler(bench.proj, "sampler11",1,list()), "There is no sampler with this name.")

  bench.runsampler(bench.proj, "sampler1",1,list(n=3,L=3))

  expect_equal(bench.proj$dataset()[1,1],"sampler1 with n 3 | L 3")
  expect_equal(bench.proj$sampled.data()[1,1], "sampler1 with n 3 | L 3")

  # run it again
  bench.runsampler(bench.proj, "sampler1",1,list(n=3,L=3))

  # with other parameter
  bench.runsampler(bench.proj, "sampler1",3,list(n=2,L=3))
  expect_equal(bench.proj$dataset()[2,1],"sampler1 with n 2 | L 3")
  expect_equal(bench.proj$sampled.data()[2,1], "sampler1 with n 2 | L 3")
  expect_equal(bench.proj$sampled.data()[4,1], "sampler1 with n 2 | L 3")

  # remove dataset
  bench.removedataset(bench.proj, "sampler1 with n 3 | L 3")
  bench.removedataset(bench.proj, "sampler1 with n 2 | L 3")

  files = list.files(bench.proj$dirbench)
  expect_equal(length(files),3)

  # remove sampler
  bench.removesampler(bench.proj, "sampler1")
  files = list.files(bench.proj$dirbench)
  expect_equal(length(files),2)
  expect_warning(bench.runsampler(bench.proj, "sampler1",1,list(n=3,L=3)), "There is no sampler with this name.")

  unlink( bench.proj$dirbench, recursive = TRUE )

})


test_that("run sampler GUI", {
  dir.name = tempdir()
  dir.create(dir.name)

  bench.proj = bench( dir.name = dir.name, new=TRUE )

  # try to add sampler
  sampler = function(parameter) {
    n = parameter$n
    L = parameter$L
    return(list(G=matrix(sample(n*L),n,L), X = matrix(sample(n),n,1), outlier = c(1)))
  }
  bench.addsampler(bench.proj, sampler, "sampler1", default.parameter = list(n=1,L=1), sampler.description =  "desc sampler1")
  bench.addsampler(bench.proj, sampler, "sampler2", default.parameter = list(n=1,L=2), sampler.description="desc sampler2")
  expect_equal(nrow(bench.proj$sampler()),2)

  # add dataset
  bench.proj = bench.adddataset(bench.proj,
                                data.G=diag(2),
                                data.X=c(1,2),
                                data.outlier = c(2),
                                data.name="data2",
                                data.description="")

  # run sampler
  bench.runsampler(bench.proj, "sampler1",1,list(n=3,L=3))
  bench.runsampler(bench.proj, "sampler2",1,list(n=3,L=3))


  #bench.samplerviewer(bench.proj)

  unlink( bench.proj$dirbench, recursive = TRUE )

})







