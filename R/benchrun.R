save.result = function(bench.proj, data.name, method.name,result) {

  dname = data.name
  mname = method.name

  if( nrow( dplyr::filter( bench.proj$dataset(), name==data.name ) )==0 ) {

    warning("Any data set exists with this name",call. = FALSE)
    return(bench.proj)
  }

  if( nrow( dplyr::filter( bench.proj$methods(), name==method.name ) )==0 ) {

    warning("Any method exists with this name",call. = FALSE)
    return(bench.proj)
  }

  result.row = dplyr::filter( bench.proj$results(), (data ==  dname) & (method ==  mname) )

  if( nrow( result.row )==0 ) {
    file.path = tempfile(tmpdir = bench.proj$dirbench)
    save(result, file = file.path)

    row = data.frame( data = I(dname),
                      method = I(mname) ,
                      file_path = I(file.path))
    add_to_table(bench.proj,row,"results")
  } else {
    save(result,file = result.row$file_path)
  }
  return(bench.proj)

}

tostring = function(parameter) {
  str = "Parameters :"
  if(length(parameter)==0){
    return(str)
  }
  nam = names(parameter)
  for( i in 1:length(parameter)){
    str=paste(str,nam[i],"=",parameter[[i]],"|")
  }
  return(str)
}

run <- function(bench.proj, data.name, method.name ) {
  dname = data.name
  mname = method.name

  load( dplyr::filter( bench.proj$methods(), name == method.name )$file_path )
  load( dplyr::filter( bench.proj$dataset(), name == data.name )$file_path )
  load( dplyr::filter( bench.proj$parameters(), (data ==  dname) & (method ==  mname) )$file_path )

  # pvalue
  ptm <- proc.time()
  result = method.func(data$G, data$X, parameter)
  t=proc.time() - ptm
  time = t[1] + t[2] + t[4] + t[5]

  save.result(bench.proj, data.name, method.name,result)
  pvalue = data.frame( pvalue = result$pvalue )
  pvalue$ind = 1:ncol(data$G)
  pvalue$outlier = pvalue$ind %in% data$outlier
  pvalue$method = method.name
  pvalue$data = data.name

  add_to_table(bench.proj,pvalue,"pvalue")

  # fdr and power
  #fdr_power = cbind( power_fdr( pvalue$p.value, data$outlier ), method = method.name, data = data.name )
  #bench.proj$fdr_power = rbind(bench.proj$fdr_power,fdr_power)

  # summary
  sum = data.frame( data = I(data.name),
                    method = I(method.name),
                    time = as.numeric(time),
                    n = nrow( data$G ),
                    L = ncol( data$G ),
                    parameter = I(tostring(parameter)) )

  add_to_table(bench.proj,sum,"summary")

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
bench.run <- function( bench.proj, data.name=NULL, method.name=NULL, again = FALSE, ...) {

  parameter = list(...)

  if( length(data.name) == 0 ) {

    data.name = as.character(bench.proj$dataset()$name)

  }

  if( length(method.name) == 0 ) {

    method.name = as.character(bench.proj$methods()$name)

  }

  # remove computations to run them again
  if( again ) {
    bench.proj = bench.remove(bench.proj, data.name, method.name)
  }


  for(d in data.name) {
    for(m in method.name) {

      cat("################################################\n")
      cat(paste("--> dataset:", d,"| method:",m,"\n"))

      if( nrow(dplyr::filter( bench.proj$summary(),
                              (data == d) & (method == m) ))>0 ) {
        # run was already call for this couple (data,method)
        warning("Done: use argument again=TRUE to run it again.\n", call. = FALSE,
                noBreaks. = TRUE, immediate. = TRUE)

      } else {
        # check if method and data frame exist
        if( length(bench.proj$dataset()) == 0 | nrow(dplyr::filter( bench.proj$dataset() ,
                                                                  name == d ))==0 ) {
          stop(paste("The data set",d,"does not exist."), call. = FALSE)

        }
        if( length(bench.proj$methods()) == 0 | nrow(dplyr::filter( bench.proj$methods() ,
                                                                  name == m ))==0 ) {
          stop(paste("The method",m,"does not exist."), call. = FALSE)

        }
        bench.addparameter(bench.proj,d,m,parameter)
        run(bench.proj,d,m)
        cat("Done.\n")
      }

    }
  }



  bench.save(bench.proj)
  return(bench.proj)
}
