

#' A convenience to quickly get the type name of a model 
#' 
#' @export
#' @param x object (model, workflow, workflowset)
#' @return string value of the model type
get_model_type <- function(x) {
  UseMethod("get_model_type")
}

#' @export
#' @rdname get_model_type
get_model_type.default = function(x){
  stop("get_model_type() is not implemented for class: ", class(x)[1])
}

#' @export
#' @rdname get_model_type
get_model_type.workflow = function(x){
  workflows::extract_spec_parsnip(x) |> 
    class() |>
    getElement(1)
}

#' @export
#' @rdname get_model_type
get_model_type.workflow_set = function(x){

  workflows::extract_spec_parsnip(x) |> 
    class() |>
    getElement(1)
}
