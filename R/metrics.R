#' Retrieve a list of known metrics
#'
#' The default set includes [yardstick::roc_auc()], [yardstick::accuracy()] and [tidysdm::tss_max()].
#' Others may be added by rebuilding the package.
#'
#' @export
#' @param x chr, one or more quoted names of existing metrics functions.  Use "all" (the default)
#'   to retrieve them all
#' @return a named list of metrics functions 
known_metrics = function(x = c("all", "roc_auc", "accuracy", "tss_max")[1]){
  
  if (!requireNamespace("tidysdm")){
    stop("please install the tidysdm package first")
  }
  
  s = list("roc_auc" = yardstick::roc_auc,
       "accuracy" = yardstick::accuracy,
       "tss_max" = tidysdm::tss_max)
  if (!("all" %in% x)) s = s[tolower(x)]
  return(s)
}

#' Build a [yarstick::metric_set()] from a list of zero or more charcater function names
#' 
#' This is a hack to allow programmatic selection of common metrics
#'
#' @seealso ["passing metrics to yarstick"](https://forum.posit.co/t/passing-metrics-into-yardstick/172004)
#' @export
#' @param x chr, one or more quoted names of existing metrics functions
#' @param metrics list of known metrics functions, see [known_metrics]
#' @return the output of [yardstick::metric_set()]
build_metric_set = function(x = c("roc_auc", "accuracy", "tss_max"),
                            metrics = known_metrics("all")){
  
  mets = metrics[tolower(x)]
  
  metrics = yardstick::metric_set(!!!mets)
  att = attr(metrics, "metrics")
  names(att) <- names(mets)
  attr(metrics, "metrics") <- att
  return(metrics)
}

