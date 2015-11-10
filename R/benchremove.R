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
      condition = paste("data = ","\'",d,"'"," AND ","method = ","\'",m,"'",sep = "")
      remove_from_table(bench.proj,condition, "pvalue")
      remove_from_table(bench.proj,condition, "summary")
      cat("Remove.\n")
    }
  }

  bench.save(bench.proj)
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

  mname = method.name

  # test input parameter
  assertthat::assert_that( assertthat::is.string( method.name )  )

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

  dname = data.name

  # test input parameter
  assertthat::assert_that( assertthat::is.string( data.name )  )
  condition = paste("name = ","\'",dname,"'",sep = "")
  remove_from_table(bench.proj,condition,"dataset")
  # ON DELETE CASCADE will remove other row !
  return(bench.proj)

}

