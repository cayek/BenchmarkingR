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
  if( !file.exists( file.path(dirbench.name,"bench.RData") ) ) {
    return(FALSE)
  }
  if( !file.exists( file.path(dirbench.name,"bench.db") ) ) {
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
                    "is not a BenchmarkingR project directory. Remove it before creating a BenchmarkingR project directory here."), call. = FALSE )
      }

      # remove the directory
      unlink( dirbench.name, recursive = TRUE )
    }
  }

  if( dir.exists( dirbench.name ) ) {

    if( !is.bencmarkingrdir(dirbench.name) ) {
      stop( paste(dirbench.name,
                  "is not a BenchmarkingR project directory. Remove it before creating a BenchmarkingR project directory here."), call. = FALSE )
    }

    #retrieve project
    load(file.path(dirbench.name,"bench.RData"))
  } else {

    # create a project
    dir.create(dirbench.name)
    bench.proj = new.env()
    bench.proj$dirbench = dirbench.name
    #create the database
    bench.proj$db = paste(dirbench.name,"bench.db",sep="")
    db <- RSQLite::dbConnect( RSQLite::SQLite() , dbname=bench.proj$db )
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE methods(
                  name TEXT PRIMARY KEY,
                  description TEXT,
                  file_path TEXT
                  )")
    RSQLite::dbClearResult(res)
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE dataset(
      name TEXT PRIMARY KEY,
      description TEXT,
      file_path TEXT
    )")
    RSQLite::dbClearResult(res)
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE summary(
      data TEXT,
      method TEXT,
      time REAL,
      n INTEGER,
      L INTEGER,
      parameter TEXT,
      PRIMARY KEY (data, method),
      FOREIGN KEY (method) REFERENCES methods(name) ON DELETE CASCADE,
      FOREIGN KEY (data) REFERENCES dataset(name) ON DELETE CASCADE
    )")
    RSQLite::dbClearResult(res)
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE pvalue(
      pvalue REAL,
      ind INTEGER,
      outlier BOOLEAN,
      method TEXT,
      data TEXT,
      FOREIGN KEY (method) REFERENCES methods(name) ON DELETE CASCADE,
      FOREIGN KEY (data) REFERENCES dataset(name) ON DELETE CASCADE
    )")
    RSQLite::dbClearResult(res)
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE results(
      data TEXT,
      method TEXT,
      file_path TEXT,
      PRIMARY KEY (data, method),
      FOREIGN KEY (method) REFERENCES methods(name) ON DELETE CASCADE,
      FOREIGN KEY (data) REFERENCES dataset(name) ON DELETE CASCADE
    )")
    RSQLite::dbClearResult(res)
    res<-RSQLite::dbSendQuery(conn = db,
                         "CREATE TABLE parameters(
      data TEXT,
      method TEXT,
      file_path TEXT,
      PRIMARY KEY (data, method),
      FOREIGN KEY (method) REFERENCES methods(name) ON DELETE CASCADE,
      FOREIGN KEY (data) REFERENCES dataset(name) ON DELETE CASCADE
    )")
    RSQLite::dbClearResult(res)
    RSQLite::dbDisconnect(db)

    # create closure
    closure.gettable <- function(tablename) {
      function() {
        con <- RSQLite::dbConnect( RSQLite::SQLite() , dbname=bench.proj$db )
        data = RSQLite::dbReadTable(conn = con,name = tablename)
        RSQLite::dbDisconnect(con)
        return(data)
      }
    }
    bench.proj$methods = closure.gettable("methods")
    bench.proj$dataset = closure.gettable("dataset")
    bench.proj$pvalue = closure.gettable("pvalue")
    bench.proj$summary = closure.gettable("summary")
    bench.proj$parameters = closure.gettable("parameters")
    bench.proj$results = closure.gettable("results")

    save(bench.proj, file = file.path(dirbench.name,"bench.RData") )

  }

  return(bench.proj)

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


power_fdr <- function( p.value, outlier ) {
  aux = sort(p.value, index.return = TRUE)$ix
  return(data.frame( fdr = sapply(0:length(p.value), function(k) { sum(!(aux[0:k] %in% outlier)) / k } ),
                     power = sapply(0:length(p.value), function(k) { sum(aux[0:k] %in% outlier) / length(outlier) } ) ) )
}




