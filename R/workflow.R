
#' Read or write a workflow given the version and path
#' 
#' @param filename str the path to the workflow
#' @return workflow
read_workflow = function(filename){
  readRDS(filename) |>
    bundle::unbundle()
}

#' @export
#' @rdname read_workflow
#' @param x workflow object
write_workflow = function(x, filename){
  ok = x |>
    bundle::bundle() |>
    saveRDS(filename)
  x
}