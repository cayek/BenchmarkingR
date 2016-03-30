remove_from_table <- function(bench.proj,condition,table) {
  con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname=bench.proj$db )
  res=RSQLite::dbSendQuery(conn = con, "PRAGMA foreign_keys = ON")
  RSQLite::dbClearResult(res)
  res=RSQLite::dbSendQuery(conn = con, paste("DELETE FROM", table, "WHERE", condition))
  RSQLite::dbClearResult(res)
  RSQLite::dbDisconnect(con)
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

      summary.row = dplyr::filter( bench.proj$summary(), method == m, data == d )

      if(nrow(summary.row)==0){
        warning("No run availables with this method and dataset",call. = FALSE)
      } else {
        condition = paste("data = ","\'",d,"'"," AND ","method = ","\'",m,"'",sep = "")
        remove_from_table(bench.proj,condition, "pvalues")
        remove_from_table(bench.proj,condition, "summary")
        cat("Remove.\n")
      }
    }
  }

  return(bench.proj)

}

#' remove method
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
bench.removemethod <- function( bench.proj, method.name ) {
  # test input parameter
  assertthat::assert_that( assertthat::is.string( method.name )  )

  # remove file
  mname = method.name
  method.row = dplyr::filter( bench.proj$methods(), name == mname )
  res.row = dplyr::filter( bench.proj$results(), method == mname )
  param.row = dplyr::filter( bench.proj$parameters(), method == mname )
  if(nrow(method.row) == 0) {

    warning("No method availables with this name",call. = FALSE)
    return(bench.proj)
  }
  # remove data file
  file.remove(method.row$file_path)
  # remove result files
  file.remove(res.row$file_path)
  # remove parameters files
  file.remove(param.row$file_path)

  condition = paste("name = ","\'",mname,"'",sep = "")
  remove_from_table(bench.proj,condition,"methods")
  # ON DELETE CASCADE will remove other row !

  return(bench.proj)

}



#' remove dataset
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
bench.removedataset <- function( bench.proj, data.name ) {

  # test input parameter
  assertthat::assert_that( assertthat::is.string( data.name )  )

  # remove file
  dname = data.name
  data.row = dplyr::filter( bench.proj$dataset(), name == dname )
  res.row = dplyr::filter( bench.proj$results(), data == dname )
  param.row = dplyr::filter( bench.proj$parameters(), data == dname )
  sampled.data.row = dplyr::filter( bench.proj$sampled.data(), data == dname )
  if(nrow(data.row) == 0) {

    warning("No data set availables with this name",call. = FALSE)
    return(bench.proj)
  }
  # remove data file
  file.remove(data.row$file_path)
  file.remove(data.row$data_exploration)
  # remove result files
  file.remove(res.row$file_path)
  # remove parameters files
  file.remove(param.row$file_path)
  # remove sampled datafile
  file.remove(sampled.data.row$file_path[!(sampled.data.row$file_path %in% data.row$file_path)])

  condition = paste("name = ","\'",dname,"'",sep = "")
  remove_from_table(bench.proj,condition,"dataset")
  # ON DELETE CASCADE will remove other row !
  return(bench.proj)

}

