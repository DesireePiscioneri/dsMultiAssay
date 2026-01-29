#' Filter a MultiAssayExperiment; server-side
#' 
#' Given a MultiAssayExperiment object, the function retains only the columns present across all experiments
#' and returns a filtered MultiAssayExperiment object.
#' Before filtering the object, the function checks the input is valid.
#'
#' @param MultiAssay A MultiAssayExperiment object.
#'
#' @return A filtered MultiAssayExperiment object, containing only complete cases.
#' @export

filterCompleteCasesDS <- function(MultiAssay){
 
  if (is.null(MultiAssay) | class(MultiAssay) != 'MultiAssayExperiment'){
    stop(paste0('Input is not valid. Please enter a MultiAssayExperiment object'))
  } 
  
  return(MultiAssayExperiment::intersectColumns(MultiAssay))
}