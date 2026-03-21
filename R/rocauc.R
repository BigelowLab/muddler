#' Generate AUC of ROC
#' 
#' @export
#' @param x fitted_workflowset or resample_results
#' @param truth,prob see [yardstick::roc_auc()] for details, not required if `x` is a last_fit_set
#' @return tibble with four elements: wflow_id, auc, roc, plot
rocauc = function(x, 
                  truth = "patch",
                  prob = ".pred_1"){
  
  if (inherits(x, c("fitted_workflowset", "resample_results"))){
    # we use lapply because group_map drops a class label
    r = lapply(seq_along(nrow(x)),
               function(irow){
                 p = x[irow,]$.predictions[[1]]
                 auc = yardstick::roc_auc(p, patch, .pred_1, event_level = "first")$.estimate
                 roc = yardstick::roc_curve(p, patch, .pred_1, event_level = "first")
                 plt =  roc |>
                   autoplot() +
                   ggplot2::labs(title = x[irow,]$wflow_id) + 
                   ggplot2::annotate("text", 0.85, 0.01, 
                                      label = sprintf("AUC: %0.3f", r$.estimate))
                 dplyr::tibble(wflow_id = x[irow,]$wflow_id,
                               auc = auc,
                               roc = list(roc), 
                               plot = list(plt))
               }) |>
      dplyr::bind_rows()
    return(r)
  } else {
    stop("not implemented for class:", paste(class(x), collapse = ", "))
  }
  
}