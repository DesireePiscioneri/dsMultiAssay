#' @title Remove biological units with missing values from a MultiAssayExperiment object
#' @description For each assay of a given MultiAssayExperiment object, it removes biological units
#' containing at least one NA.
#' 
#' @param MultiAssay a MultiAssayExperiment object;
#' @param coldatafilt logical whether to filter also colData (default is FALSE).
#'
#' @return a MultiAssayExperiment object without biological units containing NA values.
#' @export
#'
#'
MultiAssay_check_missing_dataDS <- function(MultiAssay, coldatafilt = FALSE){
  
  #Check input
  if (is.null(MultiAssay) | class(MultiAssay) != 'MultiAssayExperiment'){
    stop(paste0('Input is not valid. Please enter a MultiAssayExperiment object'))
  } 
  
  #Filter colData when 'coldatafilt = TRUE'
  if (coldatafilt == TRUE){
    MultiAssayExperiment::colData(MultiAssay) <- check_missing_dataDS(as.data.frame(MultiAssayExperiment::colData(MultiAssay)))
  }
  
  #Filter MultiAssayExperiment assays
  filtered_explist <- lapply(MultiAssayExperiment::experiments(MultiAssay), function(x){
    filtered_assay <- check_missing_dataDS(as.data.frame(t(assay(x))))
    if (class(x) == 'SummarizedExperiment'){
      x <- x[colnames(filtered_assay), rownames(filtered_assay)]
    } else if (is.data.frame(x)){
      x <- as.data.frame(t(filtered_assay))
    } else {
      x <- t(filtered_assay)
    }})
    
  #Assign the new ExperimentList
  MultiAssayExperiment::experiments(MultiAssay) <- MultiAssayExperiment::ExperimentList(filtered_explist)
  
  return(MultiAssay)
  
}

