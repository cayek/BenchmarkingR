add_to_table <- function(bench.proj,rows,tablename) {
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname=bench.proj$db )
  res=RSQLite::dbSendQuery(conn = con, "PRAGMA foreign_keys = ON")
  RSQLite::dbClearResult(res)
  RSQLite::dbWriteTable(conn = con,name = tablename, value = rows, append = TRUE)
  RSQLite::dbDisconnect(con)
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
bench.addmethod <- function(bench.proj,method.func, method.name, method.description="",replace=FALSE) {

  if (replace){
    warning("You are using replace=TRUE, only the function will be replaced. To replace the description use bench.removemethod before", call.=FALSE, noBreaks. = TRUE)
  }

  method.row = dplyr::filter( bench.proj$methods(), name==method.name )
  if( nrow( method.row )>0 ) {
    if(!replace) {
      warning("A method already exists with this name", call.=FALSE)
      return(bench.proj)
    } else {
      save(method.func,file = method.row$file_path)
      return(bench.proj)
    }
  }

  # save method
  file.path = tempfile(tmpdir = bench.proj$dirbench)
  save(method.func,file = file.path)

  row = data.frame( name = I(method.name),
                    description = I(method.description),
                    file_path = I(file.path))

  add_to_table(bench.proj, row,"methods")
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
bench.adddataset <- function(bench.proj,data.G, data.X, data.outlier, data.name, data.description="",replace=FALSE) {

  if (replace){
    warning("You are using replace=TRUE, only the function will be replaced. To replace the description use bench.removedataset before", call.=FALSE, noBreaks. = TRUE)
  }

  # create the R object
  data = list(G = data.G, X = data.X, outlier = data.outlier, name = data.name, description = data.description)


  data.row = dplyr::filter( bench.proj$dataset(), name==data.name )
  if( nrow( data.row )>0 ) {
    if(!replace) {
      warning("A data set already exists with this name",call. = FALSE)
      return(bench.proj)
    } else {
      save(data,file = data.row$file_path)
      return(bench.proj)
    }
  }

  file.path = tempfile(tmpdir = bench.proj$dirbench)
  save(data,file = file.path)

  row = data.frame( name = I(data.name),
                    description = I(data.description),
                    file_path = I(file.path))

  add_to_table(bench.proj, row,"dataset")
  return(bench.proj)
}



#' add parameter setting for data.name and method.name
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
bench.addparameter <- function(bench.proj, data.name, method.name, parameter) {

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

  param.row = dplyr::filter( bench.proj$parameters(), (data ==  dname) & (method ==  mname) )

  if( nrow( param.row )==0 ) {
    file.path = tempfile(tmpdir = bench.proj$dirbench)
    save(parameter,file = file.path)

    row = data.frame( data = I(dname),
                      method = I(mname),
                      file_path = I(file.path))
    add_to_table(bench.proj,row,"parameters")
  } else {
    save(parameter,file = param.row$file_path)
  }
  return(bench.proj)
}


