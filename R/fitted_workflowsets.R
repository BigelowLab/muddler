#' Extract predictions
#'
#' @export
#' @param x last_fit_set data frame
#' @param truth chr the name of the truth column (usually a factor)
#' @param estimate chr, the predicted probablility column name
#' @param threshold num, the threshold above which the pred probability is an event
#' @return data frame
extract_prediction = function(x, 
                              truth = "patch",
                              estimate = ".pred_class",
                              outnames = c("class", ".pred_class")){
  x |>
  dplyr::group_by(.data$wflow_id) |>
  dplyr::group_map(
    function(grp, key){
      dplyr::select(dplyr::all_of(c(truth, estimate))) |>
      rlang::set_names(outnames) |>
      dplyr::mutate(wflow_id = key$wflow_id, .before = 1)
    }) |>
    dplyr::bind_rows()
}


#' Read and write a `last_fit_set` table
#'
#' @export
#' @param x a last_fit_set date frame or one or more last_fit sets
#' @param filename chr, the name of the file to write to (default is "last_fit_set.rds")
#' @return  last_fit_set date frame
write_last_fit_set = function(x, 
                              filename = "last_fit_set.rds"){
                                
  stopifnot(inherits(x, "last_fit_set"))
  y = x
  x$.workflow = lapply(x$.workflow, bundle::bundle)
  saveRDS(x, filename)
  return(y)
}


#' Write a `last_fit_set` table
#'
#' @export
#' @rdname write_last_fit_set
read_last_fit_set = function(filename = "last_fit_set.rds"){
                                
  stopifnot(file.exists(filename))
  
  x = readRDS(filename)
  
  x$.workflow = lapply(x$.workflow, bundle::unbundle)
  
  return(x)
}

