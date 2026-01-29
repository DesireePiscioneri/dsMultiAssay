#' @title Remove Features with missing values exceeding a given threshold
#' 
#' @description For each assay of a given MultiAssayExperiment object, it removes features containing NA over a given threshold.
#' Thresholds vector can be of three types:
#'  - single value between 0 and 100: the same value will be applied to all the assays;
#'  - vector of numerical values (from 0 to 100) having the same length of MultiAssayExperiment: each value will be applied to the corresponding assay;
#'  - vector containing the names of the assay with the corresponding threshold to be applied (from 0 to 100). (e.g. 'RNASeqdataa'=50)
#'
#' @param MultiAssay A MultiAssayExperiment object;
#' @param vect_thr Vector of thresholds;
#' @param coldata_thr Integer indicating the threshold to be applied to colData (default 0).
#'
#' @return A filtered MultiAssayExperiment object
#' @export
#'

MultiAssay_preprocess_dataDS <- function(MultiAssay, vect_thr, coldata_thr = 0){
  
  #filter colData
  MultiAssayExperiment::colData(MultiAssay) <- MDR::preprocess_dataDS(as.data.frame(MultiAssayExperiment::colData(MultiAssay)), coldata_thr) 
  
  #case 1: only one threshold as input - The function applies the same threshold to all the assays
  if (length(vect_thr) == 1 && is.numeric(vect_thr)){ 
    vect_thr <- rep(vect_thr, length(MultiAssay))
  }
  
  #case 2: one threshold for each assay is given 
  if (length(vect_thr) == length(MultiAssay) && is.numeric(vect_thr)){
    filtered_explist_case2 <- mapply(function(exp, thr){
      filt_assay <- MDR::preprocess_dataDS(as.data.frame(t(assay(exp))), thr)
      if (class(exp) == 'SummarizedExperiment'){
        exp <- exp[colnames(filt_assay), rownames(filt_assay)]
      } else if (is.data.frame(exp)){
        exp <- as.data.frame(t(filt_assay))
      } else {
        exp <- t(filt_assay)
      }
      return(exp)},
      
      MultiAssayExperiment::experiments(MultiAssay),
      vect_thr,
      SIMPLIFY = F
    )
    MultiAssayExperiment::experiments(MultiAssay) <- MultiAssayExperiment::ExperimentList(filtered_explist_case2)
  
  #case 3: wrong input 
  } else if (length(vect_thr) != length(MultiAssay) && is.null(names(vect_thr))){ 
    stop('Given threshold vector must have the same length as the MultiAssayObject or assays have to be specified')
    
  #case 4: thresholds are given together with the corresponding assay names
  } else {
    filtered_explist_case4 <- mapply(function(exp, assay_name){
        if (assay_name %in% names(vect_thr)){
          filt_assaywname <- MDR::preprocess_dataDS(as.data.frame(t(assay(exp))), vect_thr[[assay_name]])
          if (class(exp) == 'SummarizedExperiment'){
            exp <- exp[colnames(filt_assaywname), rownames(filt_assaywname)]
          } else if (is.data.frame(exp)){
            exp <- as.data.frame(t(filt_assaywname))
          } else {
            exp <- t(filt_assaywname)
          }}
      return(exp)
      },
      exp <- MultiAssayExperiment::experiments(MultiAssay),
      assay_name <- names(exp),
      SIMPLIFY = F
      )
    
    MultiAssayExperiment::experiments(MultiAssay) <- MultiAssayExperiment::ExperimentList(filtered_explist_case4)}
    
   #Warning message when wrong assay names are entered
    if (!all(names(vect_thr) %in% names(MultiAssay))){
      warning(paste0(names(vect_thr)[!names(vect_thr) %in% names(MultiAssay)], ' is not an assay of MultiAssayExperiment.',  sep = "\n"))
    }
  
  return (MultiAssay)
  
}