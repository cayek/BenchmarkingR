#' Compute PCA
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
bench.pca <- function(bench.proj, data.name, again = FALSE) {

  dname = data.name
  data.row = dplyr::filter( bench.proj$dataset(), name == dname )
  if(nrow(data.row) == 0) {
    warning("No data set availables with this name",call. = FALSE)
    return(list())
  } else {
    load( data.row$data_exploration )
    load( data.row$file_path )
  }

  if(is.null(data.exploration$pca) | again) {
    # compute pca
    cat("=== Compute pca\n")
    data.exploration$pca = prcomp(data$G,scale. = FALSE)
  }

  save(data.exploration, file =data.row$data_exploration)
  return(data.exploration)
}


#' Compute sNMF
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
bench.snmf <- function(bench.proj, data.name, K, again = FALSE ) {

  dname = data.name
  data.row = dplyr::filter( bench.proj$dataset(), name == dname )
  if(nrow(data.row) == 0) {
    warning("No data set availables with this name",call. = FALSE)
    return(list())
  } else {
    load( data.row$data_exploration )
    load( data.row$file_path )
  }

  if(is.null(data.exploration$snmf) | again) {
    # compute pca
    cat("=== Compute sNMF\n")
    # create tmp file
    tmp = tempfile(tmpdir = bench.proj$dirbench)
    tmp.geno = paste(tmp,".geno", sep="")
    tmp.snmf = paste(tmp,".snmf", sep="")
    tmp.snmfProject = paste(tmp,".snmfProject", sep="")
    LEA::write.geno(data$G, tmp.geno)

    snmf.obj = LEA::snmf(tmp.geno,K=K, project = "new")
    data.exploration$snmf = list()

    # save result
    data.exploration$snmf$K = K
    data.exploration$snmf$Q = LEA::Q(snmf.obj, K = K, run = 1)
    data.exploration$snmf$G = LEA::G(snmf.obj, K = K, run = 1)

    # remove tmp files
    file.remove(tmp.geno)
    file.remove(tmp.snmfProject)
    unlink( tmp.snmf, recursive = TRUE )
  }

  save(data.exploration, file =data.row$data_exploration)
  return(data.exploration)
}
