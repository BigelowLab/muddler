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


#' Generate confusion matrices
#' 
#' Borrows heavily from the [yardstick package](https://github.com/tidymodels/yardstick/blob/51761c4e7a34e960949c75aeb2952ef02c408106/R/conf_mat.R#L421)
#' 
#' @export
#' @param x fitted_workflowset
#' @param truth,estimate see [yardstick::conf_mat()] for details, not required if `x` is a last_fit_set
#' @return tibble with three elements: wflow_id, conf_mat, plot
confmat = function(x, truth, estimate){
  
  if (inherits(x, c("fitted_workflowset", "resample_results"))){
    # we use lapply because group_map drops a class label
    r = lapply(seq_along(nrow(x)),
        function(irow){
          row = x[irow,]
          p = extract_predictions(row)
          cm = yardstick::conf_mat(p, class, .pred_class)
          dplyr::tibble(wflow_id = row$wflow_id,
                        confmat = list(cm),
                        plot = list(autoplot(cm, type = "heatmap") +
                                      ggplot2::labs(title = row$wflow_id))
                        )
        }) |>
      dplyr::bind_rows()
    return(r)
  } else {
    stop("not implemented for class:", paste(class(x), collapse = ", "))
  }
  
 #ggplot2::ggplot(data = pp,
 #                mapping = ggplot2::aes(x = .data$Truth,
 #                                       y = .data$Prediction,
 #                                       fill = .data$Freq ) ) +
 #  ggplot2::geom_tile() +
 #  ggplot2::scale_fill_gradient(low = "grey90",
 #                               high = "grey40") +
 #  ggplot2::theme(
 #    panel.background = ggplot2::element_blank(),
 #    legend.position = "none" ) +
 #  ggplot2::geom_text(
 #    mapping = ggplot2::aes(label = .data$Freq) ) +
 #  ggplot2::facet_wrap(~title)
}
