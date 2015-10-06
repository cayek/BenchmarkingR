is.bencmarkingrdir <- function( dirbench.name ) {

  if( !file.exists(dirbench.name) ) {
    return(FALSE)
  }
  if( !dir.exists(dirbench.name) ) {
    return(FALSE)
  }
  if( !file.exists( file.path(dirbench.name,"bench.RData") ) ) {
    return(FALSE)
  }

  return(TRUE)
}


#' Create a new benchmarkingR project dirrectory if the BenchmarkingR directory does not exist.
#' If the directory exits retrieve the BenchmarkingR project
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench <- function(dir.name=NULL, new=FALSE) {

  if( length(dir.name) == 0 ) {

    dir.name = "."

  }

  # test input parameter
  assertthat::assert_that( assertthat::is.string( dir.name )  )
  assertthat::assert_that( is.logical( new )  )
  assertthat::assert_that( dir.exists( dir.name )  )

  dir.name = normalizePath(dir.name)

  dirbench.name = paste(dir.name,"/BenchmarkingR_project/", sep="")

  if( new ) {

    # test if dirbench.name is a Benchmarking_project directory
    if( file.exists(dirbench.name) ) {
      if( !is.bencmarkingrdir(dirbench.name) ) {
        stop( paste(dirbench.name,
                    "is not a BenchmarkingR project directory. Remove it before creating a BenchmarkingR project directory here.") )
      }

      # remove the directory
      unlink( dirbench.name, recursive = TRUE )
    }
  }

  if( dir.exists( dirbench.name ) ) {

    if( !is.bencmarkingrdir(dirbench.name) ) {
      stop( paste(dirbench.name,
                  "is not a BenchmarkingR project directory. Remove it before creating a BenchmarkingR project directory here.") )
    }

    #retrieve project
    load(file.path(dirbench.name,"bench.RData"))
  } else {

    # create a project
    dir.create(dirbench.name)
    bench = new.env()
    bench$pvalue = data.frame()
    bench$fdr_power = data.frame()
    bench$summary = data.frame()
    bench$dataset = data.frame()
    bench$methods = data.frame()
    bench$dirbench = dirbench.name


    save(bench, file = file.path(dirbench.name,"bench.RData") )

  }

  return(bench)

}



#' save the benchmarcking project
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.save <- function(bench.proj) {
  save(bench.proj, file = file.path(bench.proj$dirbench,"bench.RData") )
}

#' add method to BenchmarkingR project
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.addmethod <- function(bench.proj,method.func, method.name, method.description="") {

  if( nrow( dplyr::filter( bench.proj$methods, name==method.name ) )>0 ) {

    stop("A method already exists with this name")

  }

  file.path = tempfile(tmpdir = bench.proj$dirbench)

  save(method.func,file = file.path)

  bench.proj$methods = rbind( bench.proj$methods, data.frame( file.path = I(file.path),
                                                              name = I(method.name),
                                                              description = I(method.description) ) )

  bench.save(bench.proj)
  return(bench.proj)

}

#' add a data set to BenchmarkingR project
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.adddataset <- function(bench.proj,data.G, data.X, data.outlier, data.name, data.description="") {

  if( nrow( dplyr::filter( bench.proj$dataset, name==data.name ) )>0 ) {

    stop("A data set already exists with this name")

  }

  # create the R object
  data = list(G = data.G, X = data.X, outlier = data.outlier, name = data.name, description = data.description)

  file.path = tempfile(tmpdir = bench.proj$dirbench)

  save(data,file = file.path)

  bench.proj$dataset = rbind( bench.proj$dataset, data.frame( file.path = I(file.path),
                                                              name = I(data.name),
                                                              description = I(data.description) ) )

  bench.save(bench.proj)
  return(bench.proj)
}


power_fdr <- function( p.value, outlier  ) {
  aux = sort(p.value, index.return = TRUE)$ix
  return(data.frame( fdr = sapply(0:length(p.value), function(k) { sum(!(aux[0:k] %in% outlier)) / k } ),
                     power = sapply(0:length(p.value), function(k) { sum(aux[0:k] %in% outlier) / length(outlier) } ) ) )
}


run <- function(bench.proj, data.name, method.name) {

  load( dplyr::filter( bench.proj$methods, name == method.name )$file.path )
  load( dplyr::filter( bench.proj$dataset, name == data.name )$file.path )

  # pvalue
  pvalue = data.frame( p.value = method.func(data$G, data$X) )
  pvalue$ind = 1:ncol(data$G)
  pvalue$outlier = pvalue$ind %in% data$outlier
  pvalue$method = method.name
  pvalue$data = data.name

  bench.proj$pvalue = rbind(bench.proj$pvalue,pvalue)

  # fdr and power
  fdr_power = cbind( power_fdr( pvalue$p.value, data$outlier ), method = method.name, data = data.name )

  bench.proj$fdr_power = rbind(bench.proj$fdr_power,fdr_power)

  # summary
  sum = data.frame( data = data.name, method = method.name, time = 0.0, n = nrow( data$G ), L = ncol( data$G ) )

  bench.proj$summary = rbind(bench.proj$summary,sum)

}


#' run bencmarking
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.run <- function( bench.proj, data.name=NULL, method.name=NULL, again = FALSE) {

  if( length(data.name) == 0 ) {

    data.name = as.character(bench.proj$dataset$name)

  }

  if( length(method.name) == 0 ) {

    method.name = as.character(bench.proj$methods$name)

  }

  # remove computations to run them again
  if( again ) {
    bench.proj = bench.remove(bench.proj, data.name, method.name)
  }


  for(d in data.name) {
    for(m in method.name) {

      cat("################################################\n")
      cat(paste("--> dataset:", d,"| method:",m,"\n"))

      if( nrow(dplyr::filter( bench.proj$summary,
                              (data == d) & (method == m) ))>0 ) {
        # run was already call for this couple (data,method)
        warning("Done: use argument again=TRUE to run it again.\n", call. = FALSE,
                noBreaks. = TRUE, immediate. = TRUE)

      } else {
        # check if method and data frame exist
        if( length(bench.proj$dataset) == 0 | nrow(dplyr::filter( bench.proj$dataset ,
                                                                  name == d ))==0 ) {
          stop(paste("The data set",d,"does not exist."))

        }
        if( length(bench.proj$methods) == 0 | nrow(dplyr::filter( bench.proj$methods ,
                                                                  name == m ))==0 ) {
          stop(paste("The method",m,"does not exist."))

        }
        run(bench.proj,d,m)
        cat("Done.\n")
      }

    }
  }



  bench.save(bench.proj)
  return(bench.proj)
}




#' remove specified already computed benchmark
#'
#'
#' TODO
#'
#' @return TODO
#'
#' @examples
#' TODO
#
#' @export
bench.remove <- function( bench.proj, data.name, method.name) {

  for(d in data.name) {
    for(m in method.name) {
      cat("################################################\n")
      cat(paste("--> dataset:", d,"| method:",m,"\n"))
      bench.proj$summary = dplyr::filter( bench.proj$summary,
                                          (data != d) | (method != m) )
      bench.proj$pvalue = dplyr::filter( bench.proj$pvalue,
                                         (data != d) | (method != m) )
      bench.proj$fdr_power = dplyr::filter( bench.proj$fdr_power,
                                            (data != d) | (method != m) )
      cat("Remove.\n")
    }
  }

  bench.save(bench.proj)
  return(bench.proj)

}
