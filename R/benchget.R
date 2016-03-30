#' Getter of result of a run
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
bench.getresult <- function( bench.proj, data.name, method.name) {

  dname = data.name
  mname = method.name

  result.row = dplyr::filter( bench.proj$results(), (data ==  dname), (method ==  mname) )

  if(nrow(result.row) == 0) {

    warning("No result available for this run",call. = FALSE)
    return(list())
  } else {
    load( result.row$file_path )
    return(result)
  }
}

#' Getter of parameters of a run
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
bench.getparam <- function( bench.proj, data.name, method.name) {
  dname = data.name
  mname = method.name

  param.row = dplyr::filter( bench.proj$parameters(), (data ==  dname) & (method ==  mname) )

  if(nrow(param.row) == 0) {

    warning("No result availables for this run",call. = FALSE)
    return(list())
  } else {
    load( param.row$file_path )
    return(parameter)
  }
}


#' Getter a data.exploration
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
bench.getdata.exploration <- function( bench.proj, data.name) {
  dname = data.name
  data.row = dplyr::filter( bench.proj$dataset(), name == dname )

  if(nrow(data.row) == 0) {

    warning("No data set availables with this name",call. = FALSE)
    return(list())
  } else {
    load( data.row$data_exploration )
    return(data.exploration)
  }
}

#' Getter a dataset
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
bench.getdata <- function( bench.proj, data.name) {
  dname = data.name
  data.row = dplyr::filter( bench.proj$dataset(), name == dname )

  if(nrow(data.row) == 0) {

    warning("No data set availables with this name",call. = FALSE)
    return(list())
  } else {
    load( data.row$file_path )
    return(data)
  }
}


#' Getter a method
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
bench.getmethod <- function( bench.proj, method.name) {
  mname = method.name
  method.row = dplyr::filter( bench.proj$methods(), name == mname )

  if(nrow(method.row) == 0) {

    warning("No method availables with this name",call. = FALSE)
    return(list())
  } else {
    load( method.row$file_path )
    return(method.func)
  }
}

