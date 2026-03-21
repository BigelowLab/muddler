#' Extract predictions
#'
#' @export
#' @param x fitted workflow or fitted_workflowset data frame
#' @param truth chr the name of the truth column (usually a factor)
#' @param estimate chr, the predicted probablility column name
#' @param outnames chr, names the output data frame should have
#' @return data frame 
extract_predictions = function(x, 
                              truth = "patch",
                              estimate = ".pred_class",
                              outnames = c("class", ".pred_class")){
  if(FALSE){
    truth = "patch"
    estimate = ".pred_class"
    outnames = c("class", ".pred_class")
  }
  #stopifnot(inherits(x, c("fitted_workflowset", "resample_results")))
  stopifnot(all(c(".predictions", "wflow_id") %in% names(x)))
  x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(grp, key){
        dplyr::select(grp$.predictions[[1]], dplyr::all_of(c(truth, estimate))) |>
              rlang::set_names(outnames) |>
        dplyr::mutate(wflow_id = grp$wflow_id, .before = 1)
      }, .keep = TRUE) |>
      dplyr::bind_rows()
}


#' Read and write a `fitted_workflowset` table
#'
#' @export
#' @param x a last_fit_set date frame or one or more last_fit sets
#' @param filename chr, the name of the file to write to (default is "last_fit_set.rds")
#' @return  last_fit_set date frame
write_fitted_workflowset = function(x, 
                              filename = "fitted_workflowset.rds"){
                                
  stopifnot(inherits(x, "fitted_workflowset"))
  y = x
  x$.workflow = lapply(x$.workflow, bundle::bundle)
  saveRDS(x, filename)
  return(y)
}


#' Write a `fitted_workflowset` table
#'
#' @export
#' @rdname write_fitted_workflowset
read_last_fit_set = function(filename = "fitted_workflowset.rds"){
                                
  stopifnot(file.exists(filename))
  
  x = readRDS(filename)
  
  x$.workflow = lapply(x$.workflow, bundle::unbundle)
  
  return(x)
}

