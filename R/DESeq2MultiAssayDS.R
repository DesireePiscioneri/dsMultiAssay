#' Differential expression analysis on a MultiAssayExperiment; server-side
#' 
#' Given a MultiAssayExperiment object and the name of one of its SummarizedExperiment,
#' the function performs a Differential gene expression analysis using \code{DESeq2} package.
#' 
#' @param vars variables for the model;
#' @param MultiAssay a MultiAssayExperiment object;
#' @param SE a character string containing the name of a SummarizedExperiment object;
#' @param test "Wald" or "LRT";
#' @param fitType "parametric", "local", or "mean";
#' @param sfType "ratio", "poscounts", or "iterate";
#' @param reduced test="LRT";
#' @param contrast model matrix contrast.
#' 
#' @import dplyr
#' 
#' @export
#'
DESeq2MultiAssayDS<-function(vars, MultiAssay, SE, test, fitType, sfType, reduced, contrast)
{  
  
  set <- getWithColData(MultiAssay, SE)
  
  if(is.null(set) | class(set) != "SummarizedExperiment"){
    stop(paste0(SE, 'is not a valid experiment for', MultiAssay, '. Please enter a SummarizedExperiment object.'))
  } 
  
  #design formula
  vars <- unlist(strsplit(vars, split=","))
  set <- set[, complete.cases(as.data.frame(colData(set))[, vars, drop = FALSE])]
  warning("Samples containing NA in the design variables were removed")
  ff <- paste("~", paste(c(vars), collapse="+"))
  
  #convert data into DESeqDataSet
  dds <- DESeq2::DESeqDataSet(se = set, 
                              design = stats::formula(ff))
  
  if(is.null(reduced))
  {
    dds<-DESeq2::DESeq(dds, test = test, fitType = fitType, sfType = sfType)  
  }else{
    reduced <- unlist(strsplit(reduced, split=","))
    reduced <- paste("~", paste(c(reduced), collapse="+"))
    dds <- DESeq2::DESeq(dds,test = test, fitType = fitType,
                         sfType = sfType, reduced = stats::formula(reduced)) 
  }
  
  if(is.null(contrast)){
    res <- DESeq2::results(dds) 
  } else{
    contrast<-unlist(strsplit(contrast, split=","))
    res <- DESeq2::results(dds, contrast = contrast) 
  }
  # Gene names (rownames) to column to avoid loosing them when converting to tibble
  res <- tibble::rownames_to_column(data.frame(res), "gene")
  return(as_tibble(res))
}