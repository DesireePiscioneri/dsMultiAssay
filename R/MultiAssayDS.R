
#' @export
MultiAssayDS <- function(exp, df = NULL, df_col = NULL, colD = NULL, 
                         levels_col = NULL, level = NULL,
                         sampleMap = NULL,
                         metadata = list(), drops = list()){


  #Create/update colData
  if (!is.null(df)) {
    
    if (is.null(df_col)){
      stop(paste0('Please enter a valid column name')) 
    } 
    
    else { 
      new_coldata <- split_dfDS(df, df_col)[[2]]
      
      if (is.null(colD)){
        colD <- new_coldata
      }
      
      else {
        new_coldata <- new_coldata[order(rownames(new_coldata)), ]
        colD <- cbind(new_coldata, colD)
      }
      
      #Add new assays to exp
      df <- split_dfDS(df, df_col)[[1]]
      
      if (!is.null(levels_col)){
        
        if (!(levels_col %in% names(df))){ #check that input col is in the dataframe
          stop(paste0('Please enter a valid column name '))
        }
        
        if (is.null(level)){ #when no levels are given as input, it takes all the possible ones from the column
          level <- levels(factor(df[[levels_col]])) 
        }
        
        if (all(level %in% df[[levels_col]])){ 
          assay_NA <- as.data.frame(df[is.na(df[levels_col]),])
          
          if(any(duplicated(assay_NA[[df_col]]))){ 
            print(assay_NA[[df_col]][duplicated(assay_NA[[df_col]])])
            stop("Duplicated patients detected in NA Assay")
          }
          
          rownames(assay_NA)<-assay_NA[[df_col]]
          assay_NA[df_col] <- NULL
          list_assays <- list() 
          list_assays <- c(list_assays, list(column_na=assay_NA)) 
          names(list_assays)[names(list_assays) == "column_na"] <- paste0(levels_col, '_', 'NA')
          df<-df[!is.na(df[[levels_col]]),] 
          
          for (value in level) {
            new_assay <- as.data.frame(df[df[levels_col] == value,]) 
            
            if(any(duplicated(new_assay[[df_col]]))){
              print(new_assay[[df_col]][duplicated(new_assay[[df_col]])])
              stop("Duplicated patients detected in new_assay")
            }
            
            rownames(new_assay)<-new_assay[[df_col]]
            new_assay[df_col] <- NULL
            list_assays <- c(list_assays, list(value=new_assay)) 
            names(list_assays)[names(list_assays) == "value"] <- paste0(levels_col, '_', value)
          } 
          
          list_assays <- lapply(list_assays, function(x) as.data.frame(t(x)))
          exp <- c(exp, list_assays)
        }
        
        else {
          stop(paste0('Wrong levels. Please enter valid levels for variable ', levels_col))
        }}}}

  if (is.null(colD)){
    if (is.null (sampleMap)){
      ma <- MultiAssayExperiment::MultiAssayExperiment(experiments = exp, 
                                                       metadata = metadata, 
                                                       drops = drops)
    } else {
      ma <- MultiAssayExperiment::MultiAssayExperiment(experiments = exp, 
                                                       sampleMap = sampleMap,
                                                       metadata = metadata, 
                                                       drops = drops)
    }
  } else if (is.null(sampleMap)){
    ma <- MultiAssayExperiment::MultiAssayExperiment(experiments = exp,
                                                     colData = colD,
                                                     metadata = metadata, 
                                                     drops = drops)
  } else {
    ma <- MultiAssayExperiment::MultiAssayExperiment(experiments = exp, 
                                                     colData = colD, 
                                                     sampleMap = sampleMap, 
                                                     metadata = metadata, 
                                                     drops = drops)
  }
  
  return(ma)
  
}
