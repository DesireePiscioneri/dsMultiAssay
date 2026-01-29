#' Split a Data frame into variable and constant columns; server-side
#' 
#' Given a data frame and a reference grouping column, the function aggregates all rows of the same group,
#' identifying, for each group, variable and constant columns.
#' The function returns a list containing two data frames:
#' - filtered_df, i.e. the original data frame with constant columns removed;
#' - filtered_coldata, i.e. a new data frame containing only the constants columns.
#'
#' @param df A data frame to be processed
#' @param column A character string indicating the name of the grouping column
#'
#' @keywords internal
#' @return A list of two data frames 
#' @export
#' 
split_dfDS <- function(df, column){
  
  aggregate_df <- aggregate(df[,which(names(df)!=column)], by = list(column = df[[column]]),
                   FUN = function(x) length(unique(x)) == 1)
  
  constant_col <- colSums(aggregate_df[,-1]) == nrow(aggregate_df) 
  constant_names <- names(which(constant_col==TRUE))
  
  coldata_df <- df[, c(column, constant_names), drop = FALSE]
  coldata_df <- aggregate(coldata_df[,which(names(coldata_df)!=column)], by = list(column = coldata_df[[column]]), unique)
  rownames(coldata_df)<-coldata_df[,1] 
  coldata_df[,1]<-NULL
  
  df[,constant_names]<-NULL
  
  return(list(filtered_df = df, filtered_coldata = coldata_df))
  }

