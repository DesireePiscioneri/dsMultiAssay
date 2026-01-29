#'Extract Experiments from a MultiAssayExperiment; server-side
#'
#'Given a MultiAssayExperiment object, the function returns the corresponding ExperimentList 
#'containing the experiments data.
#'
#' @param Multiassay A non NULL MultiAssayExperiment object.
#'
#' @return An ExperimentList object 
#' 
#' @export
 
experimentsDS <-  function(Multiassay){
  
  if (is.null(Multiassay) | class(Multiassay) != 'MultiAssayExperiment'){
    stop(paste0('Input is not valid. Please enter a MultiAssayExperiment object'))
  }
  
  return(MultiAssayExperiment::experiments(Multiassay))
}


