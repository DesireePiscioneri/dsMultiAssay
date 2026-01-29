#' @title Create a MultiAssayExperiment Object
#' @description Given a list of experiments, optional colData, sampleMap and metadata, the function constructs a MultiAssayExperiment object. 
#' It can also integrate data from an input data frame, extracting colData and splitting the remaining data into multiple assays based on factor levels.
#' 
#' @param exp A list or ExperimentList of experiments;
#' @param df Optional data frame to be processed into colData or new assays;
#' @param df_col character, the column name in `df` containing biological units identifiers;
#' @param colD dataframe with characteristics of biological units;
#' @param levels_col character, the column in `df` used to split data into multiple assays;
#' @param level optional Vector of specific levels to be extracted from `levels_col`;
#' @param sampleMap A data frame of assay names, sample identifiers, and colname samples;
#' @param metadata Optional list of metadata;
#' @param drops Optional list for dropping elements.
#' 
#' @return A MultiAssayExperiment object
#' 
#' 
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
          # Check for duplicate patient IDs in the NA group
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
          
          #Iterate through levels to create individual assays
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
  
  #MultiAssayExperiment construction
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
