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
stop("to TODO")
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

#' plot all res of the computation
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
bench.plotres <- function(bench.proj, data.name, method.name) {

  ret = list()

  res = bench.getresult(bench.proj, data.name, method.name)
  data = bench.getdata(bench.proj, data.name)

  index = 1:ncol(data$G)

  if(!is.null(res$pvalue)) {
    # manhattan plot
    ret$mplot = ggplot2::ggplot( data.table::data.table(index = index, pvalue = res$pvalue, outlier = index %in% data$outlier),
                                 ggplot2::aes(x=index, y = -log(pvalue), color=outlier) ) +
      ggplot2::geom_point() +
      ggplot2::geom_point( ggplot2::aes(x = index, y = -log(pvalue), color=outlier, size=2), subset = plyr::.(outlier == TRUE) ) +
      ggplot2::xlab("locus index") +
      ggplot2::guides(color=FALSE,size=FALSE) +
      ggplot2::ggtitle(paste("Manhattan plot of",data.name,"computed wih",method.name))

    # histogram
    ret$histpvalue = ggplot2::ggplot( data.table::data.table(index = index, pvalue = res$pvalue, outlier = index %in% data$outlier),
                                      ggplot2::aes(x = pvalue, fill=outlier) ) +
      ggplot2::geom_bar() +
      ggplot2::guides(color=FALSE,size=FALSE, fill = FALSE) +
      ggplot2::ggtitle(paste("Histogram of pvalue of",data.name,"computed wih",method.name))
  }

  if(!is.null(res$B)) {

    ret$b = ggplot2::ggplot( data.table::data.table(index = index, b = as.numeric(res$B), outlier = index %in% data$outlier),
                                 ggplot2::aes(x=index, y = b, color=outlier) ) +
      ggplot2::geom_point() +
      ggplot2::geom_point( ggplot2::aes(x = index, y = b, color=outlier, size=2), subset = plyr::.(outlier == TRUE) ) +
      ggplot2::xlab("locus index") +
      ggplot2::guides(color=FALSE,size=FALSE) +
      ggplot2::ggtitle(paste("coef",data.name,"computed wih",method.name))

  }

return(ret)
}

#' Plot fdr and power of method for this data.name. Use B-H procedure for controlling the FDR
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
bench.plotfdr_power <- function(bench.proj, data.name, methods.name ,fdr_lim=c(0.0,0.25) ) {

  # retrieve pvalue
  pvalues = dplyr::filter( bench.proj$pvalues(), method %in% methods.name, data == data.name)

  # plots
  p1 = ggplot2::ggplot(pvalues, ggplot2::aes(x = threshold, y = fdr, color=method)) + ggplot2::geom_line() +
    ggplot2::xlim(fdr_lim) +
    ggplot2::geom_abline(intercept = 0, slope = 1) + ggplot2::ylim(c(0.0,1.0)) + ggplot2::xlab("qvalue computed with package qvalue")

  p2 = ggplot2::ggplot(pvalues, ggplot2::aes(x = threshold, y = power, color=method)) + ggplot2::geom_line() +
    ggplot2::xlim(fdr_lim) + ggplot2::xlab("qvalue computed with package qvalue")


  p3 = ggplot2::ggplot(pvalues, ggplot2::aes(x = power, y = (1-fdr), color=method)) + ggplot2::geom_line() +
      ggplot2::scale_y_log10(breaks=c(1e-2,1e-1,1.0),limits = c(1e-3,1.0))

  p4 = ggplot2::ggplot(pvalues, ggplot2::aes(x = threshold, y = 2 * (1-fdr) * power / ((1-fdr) + power), color=method)) +
    ggplot2::geom_line() +
    ggplot2::xlab("qvalue computed with package qvalue") +
    ggplot2::ylab("F1 score") +
    ggplot2::xlim(fdr_lim)

  return(list(fdr = p1, power = p2, fdr.power = p3, f1.score = p4))

}

#' Compute a heatmap of cerrelation between methods pvalue
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
bench.heatmap <- function(bench.proj, data.name, methods.name ) {

  # retrieve pvalue
  pvalues =  dplyr::select(tidyr::spread(
    dplyr::select(
      dplyr::filter(
        bench.proj$pvalues(), method %in% methods.name, data == data.name
      ),
      pvalue, method, ind
    ),
    method, pvalue
  ), -ind)

cor.matrix = cor(pvalues)

  p1 = ggplot2::ggplot(reshape2::melt(cor.matrix), ggplot2::aes(x = Var1, y = Var2)) + ggplot2::geom_tile(ggplot2::aes(fill = value)) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue", limits=c(0, 1))
  return(list(cor = p1))

}

