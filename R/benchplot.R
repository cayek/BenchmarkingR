##########################################################################
######################                   #################################
######################   PLOT FUNCTIONS  #################################
######################                   #################################
##########################################################################

#' manhattan plot
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
bench.manhanttanplot <- function(bench, data.name, method.name ) {

  p = ggplot2::ggplot( filter(bench$pvalue, method == method.name, method == method.name),
                       aes(x=ind, y = -log(p.value), color=outlier) ) +
    geom_point() +
    geom_point( aes(x = ind, y = -log(p.value), color=outlier, size=2), subset = .(outlier == TRUE) ) +
    xlab("locus index") +
    guides(color=FALSE,size=FALSE) +
    ggtitle(paste("Manhattan plot of",data.name,"computed wih",method.name))

  return(p)
}

bench.hist <- function(bench, data.name, method.name ) {

  p = ggplot( filter(bench$pvalue, method == method.name, method == method.name),
              aes(x = p.value, color=outlier, fill=outlier) ) +
    geom_bar() +
    guides(color=FALSE,fill=FALSE) +
    ggtitle(paste("Histogram of",data.name,"p values computed wih",method.name))

  return(p)
}

bench.dfr_power <- function(bench, data.name) {
  p = ggplot(filter(bench$fdr_power, data == data.name ), aes(x = fdr, y = power, group=method, color=method)) + geom_smooth() + geom_abline() + xlim(c(0,1)) + ylim(c(0,1.1))
  return(p)
}

#' Use qvalue package to compute expected fdr then plot threshold against power and fdr
#'
#' @export
bench.power_fdr_threshold <- function(p.value, outlier) {
  q.value = qvalue::qvalue(p.value)$qvalue
  cat("TODO")


}
