#' Statistics of Complete cases in a MultiAssayExperiment; server-side
#'
#' Given a MultiAssayExperiment object, the function returns a frequency table containing the number of 
#' complete cases across all experiments.
#'
#' @param MultiAssay a MultiAssayExperiment object.
#'
#' @return A table with the number of incomplete and complete cases
#' 
#' @export

statCompleteCasesDS <- function(MultiAssay){
  
  if (is.null(MultiAssay) | class(MultiAssay) != 'MultiAssayExperiment'){
    stop(paste0('Input is not valid. Please enter a MultiAssayExperiment object'))
  } 
  
  return(table(MultiAssayExperiment::complete.cases(MultiAssay))) 
}