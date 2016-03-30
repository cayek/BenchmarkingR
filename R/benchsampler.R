parameterToString <- function(parameter) {
  if(length(parameter) == 0) {
    return("parameter = list()")
  }

  s = "parameter = list("
  for(n in names(parameter)) {
    s = paste(s, n, "=",parameter[n],",")
  }
  s = substring(s,1,nchar(s)-2)
  s = paste(s,")")
  return(s)
}



#' add sampler to BenchmarkingR project
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
bench.addsampler <- function(bench.proj,sampler.func, sampler.name, default.parameter = list(), sampler.description="") {

  sampler.row = dplyr::filter( bench.proj$sampler(), name==sampler.name )
  if( nrow( sampler.row )>0 ) {
    warning("A sampler already exists with this name. Use bench.removesampler before adding this.", call.=FALSE)
    return(bench.proj)
  }

  # save method
  file.path = tempfile(tmpdir = bench.proj$dirbench)
  save(sampler.func, file = file.path)

  row = data.frame( name = I(sampler.name),
                    description = I(sampler.description),
                    file_path = I(file.path),
                    default_parameter = I(parameterToString(default.parameter)))

  add_to_table(bench.proj, row,"sampler")
  return(bench.proj)

}

listToString <- function(l){
  s = ""
  for(n in names(l)) {
    s = paste(s,"|", n, l[n])
  }
  return(substring(s,4))



  }


#' add sampler to BenchmarkingR project
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
bench.runsampler <- function(bench.proj, sampler.name, repetition, parameter) {

  sampler.row = dplyr::filter( bench.proj$sampler(), name==sampler.name )
  if( nrow( sampler.row )==0 ) {
    warning("There is no sampler with this name.", call.=FALSE)
    return(bench.proj)
  }

  assertthat::assert_that(repetition>=1)

  data.name = paste(sampler.name, "with", listToString(parameter))
  # test the sampler was already run with this parameter
  data.row = dplyr::filter( bench.proj$dataset(), name==data.name )
  if( nrow( data.row )>0 ) {
    warning("A dataset already exist as sampling with these parameters. Remove it before running the sampler.",call. = FALSE)
    return(bench.proj)
  }

  # load sampler
  load(sampler.row$file_path)

  # run sampler
  ## dataset table
  dataexploration.file.path = tempfile(tmpdir = bench.proj$dirbench)
  data.exploration = list()
  save(data.exploration, file = dataexploration.file.path)
  dataset.row = data.frame( name = I(data.name),
                            description = I(paste("Simulation run with", sampler.name, "with parameter :", listToString(parameter))),
                            file_path = I("file.path"),
                            ata_exploration = I(dataexploration.file.path))
  ## sampled_data table
  sampled_data.row = data.frame( data = data.name,
                                 file_path = I("file.path"),
                                 number = 1:repetition )
  cat("################################################\n")
  cat(paste("--> sampler:", sampler.name,"\n"))
  for(rep in 1:repetition) {
    file.path = tempfile(tmpdir = bench.proj$dirbench)
    # run
    parameter$rep = rep
    data = sampler.func(parameter)
    save(data, file = file.path)
    sampled_data.row[rep,2] = file.path
    cat("Done:",rep,"\n")
  }
  dataset.row[1,3]=sampled_data.row[1,2]

  add_to_table(bench.proj, dataset.row,"dataset")
  add_to_table(bench.proj, sampled_data.row,"sampled_data")
  return(bench.proj)

}



#' remove sampler
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
bench.removesampler <- function( bench.proj, sampler.name ) {
  # test input parameter
  assertthat::assert_that( assertthat::is.string( sampler.name )  )

  # remove file
  sampler.row = dplyr::filter( bench.proj$sampler(), name == sampler.name )
  if(nrow(sampler.row) == 0) {

    warning("No sampler availables with this name",call. = FALSE)
    return(bench.proj)
  }
  # remove sampler func
  file.remove(sampler.row$file_path)


  condition = paste("name = ","\'",sampler.name,"'",sep = "")
  remove_from_table(bench.proj,condition,"sampler")
  # ON DELETE CASCADE will remove other row !

  cat("Remark: if dataset exist as sampling from this sampler there are not remove with the sampler. You must remove them with bench.removedataset.\n")

  return(bench.proj)

}


