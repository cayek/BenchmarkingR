library(BenchmarkingR)
library(ssh.utils)
context("run on patator")

test_that("Run on a remote session on patator", {
  dir.name = basename(tempdir())
  run.remote(cmd = paste("mkdir /home/cayek/tmp/",dir.name,sep=""), remote = "cayek@patator.imag.fr")

  pc.dirname = paste("/home/cayek/PatatorHomeDir/tmp/",dir.name,sep="")
  patator.dirname = paste("/home/cayek/tmp/",dir.name,sep="")

  bench.proj = bench( dir.name = pc.dirname, new=TRUE, patatorDir = patator.dirname )


  # add data and method
  function.test = function(G,X,parameter) {
    return(list(pvalue = 1:ncol(G),K = parameter$K))
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


  # TODO
  #torun = run_on_patator(bench.proj$patatorDir)

  #save(torun,file = paste(bench.proj$dirbench,"/torun",sep=""))

})
