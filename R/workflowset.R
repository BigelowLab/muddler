#' Selects best hyperparams, transfers those to the final model and then 
#' performs a final fit and testing suite for each wflow in the set.
#' 
#' Note that this wraps a number of important steps into one.  We provided this
#' in the interest of speed, but the details are worthy of investigating.
#' 
#' @param wflow a workflow set with one or more workflows
#' @param splitdata rsplit object with training and testing data
#' @return a data frame of class "last_fit_set" with one or more  "last_fit" model objects 
workflowset_selectomatic = function(wflow, splitdata){
  
  # fish out the best metrics per workflow
  m = workflowset_metrics(wflow, what = "best")
  
  wflows = wflow$wflow_id |>
    sapply(
      function(wid){
        # get the workflow
        w = workflowsets::extract_workflow(wflow, wid)
        # transfer the "best" hyperparameter values to each workflow
        these_metrics = dplyr::filter(m, .data$wflow_id == wid)
        tune::finalize_workflow(w, these_metrics) |>
          # now fit with all of the training data (and testing data for measuring)
          tune::last_fit(splitdata, metrics = tidysdm::sdm_metric_set(yardstick::accuracy))
      }, simplify = FALSE) |> 
    dplyr::bind_rows() |>
    dplyr::mutate(wflow_id =  wflow$wflow_id, .before = 1)

  class(wflows) <- c("last_fit_set", class(wflows))
  return(wflows)
}


#' Function that takes a workflow set and returns a metric table,
#' possibly selecting hyperparameter set with the best average metric.
#' 
#' @param wf_set a workflow set
#' @param wflow_id str, one or more workflow ids, by deafult all are computed
#' @param what str, currently accepts only "best" to chose the best set, otherwise
#'   it returns all of the hyperparameters unranked.
#' @return a table of models metrics, possible ranked by the average mean of metrics
workflowset_metrics = function(wf_set, 
                        wflow_id = dplyr::pull(wf_set, dplyr::all_of("wflow_id")),
                        what = "best"){

  # call recursively one id at a time
  if (length(wflow_id) > 1){
    rr = sapply(wflow_id,
           function(wid){
             workflowset_metrics(wf_set, wid, what = what) |>
               dplyr::mutate(wflow_id = wid, .before = 1)
           }, simplify = FALSE) |>
      dplyr::bind_rows()
    return(rr)
  }
  
  metrics = wf_set$result[[1]]$.metrics[[1]] |> 
    dplyr::pull(dplyr::all_of(".metric"))
  
  keep_me = c("wflow_id", ".config", metrics)
  m =  wf_set |>
    workflowsets::extract_workflow_set_result(wflow_id) |>
    tune::collect_metrics(summarize = TRUE, type = "wide") |>
    dplyr::mutate(wflow_id = wflow_id, .before = 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(mean = mean(dplyr::c_across(dplyr::any_of(metrics) ))) |>
    dplyr::arrange(dplyr::desc(mean))
  
  if (tolower(what[1]) == "best"){
    m = dplyr::group_by(m, wflow_id) |>
      dplyr::slice_head(n = 1) |>
      dplyr::ungroup()
  }
  m
}



