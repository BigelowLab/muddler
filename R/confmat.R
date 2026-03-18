#' Convert a confusion matrix (conf_mat class) to a tibble
#'
#' @export
#' @param cm conf_mat class object
#' @return data frame (tibble) of the confusion matrix
confmat_as_tibble = function(cm){
  x = as.data.frame.table(cm$table) |>
    rlang::set_names(c("Prediction", "Truth", "Freq"))
  lvls = levels(x$Prediction)
  dplyr::as_tibble(x) |>
    dplyr::mutate(Prediction = factor(.data$Prediction, levels = rev(lvls)),
                  Truth = factor(.data$Truth, levels = lvls))
}


#' Plot confusion matrices
#' 
#' Borrows heavily from the [yardstick package](https://github.com/tidymodels/yardstick/blob/51761c4e7a34e960949c75aeb2952ef02c408106/R/conf_mat.R#L421)
#' 
#' @param x fitted workflow or fitted workflow_set
#' @param truth,estimate see [yardstick::conf_mat()] for details, not required if `x` is a last_fit_set
#' @return tibble with three elements: workflow, confmat, plot
confmat = function(x, truth, estimate,
                   ...){
  
  if (inherits(x, "last_fit_set")){
    r = dplyr::rowwise(x) |>
      dplyr::group_map(
        function(row, key){
          wflow = workflowsets::extract_workflow(row, row$wflow_id[1])
          confmat()
        }, keep = TRUE) |>
      dplyr::bind_rows()
    return(r)
  }
  
  mold = workflows::extract_mold(x)
  newdata = recipes::bake(workflows::extract_recipe(x), mold$predictors)
  outcomes = mold$outcomes |>
    rlang::set_names("class")
  
  acc = yardstick::accuracy(outcomes, class, .pred_class)
  
  pp = x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        wid = row |> dplyr::pull(1)
        row$.predictions[[1]] |>
          yardstick::conf_mat(class, .pred_class) |>
          confmat_as_tibble() |>
          dplyr::mutate(wflow_id = wid,
                        .before = 1) |>
          dplyr::mutate(.accuracy = dplyr::filter(acc, .data$wflow_id == wid) |>
                          dplyr::pull(dplyr::all_of(".estimate")))
        
      }) |> 
    dplyr::bind_rows() |>
    dplyr::mutate(title = sprintf("%s (acc = %0.3f)", .data$wflow_id, .data$.accuracy))
  
  ggplot2::ggplot(data = pp,
                  mapping = ggplot2::aes(x = .data$Truth,
                                         y = .data$Prediction,
                                         fill = .data$Freq ) ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "grey90",
                                 high = "grey40") +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      legend.position = "none" ) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = .data$Freq) ) +
    
    ggplot2::facet_wrap(~title)
}
