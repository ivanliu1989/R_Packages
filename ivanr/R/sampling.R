
#' Function to do stratified sampling for model data
#'
#' Function to stratified split a dataset and return training, test and eval sets as data.frame or data.table
#'
#' @param x A data.frame or data.table object containing all data to be split
#' @param strataCols Names of columns or features that will be used to do stratification
#' @param trainProportion Proportion of training set
#' @param evalProportion Proportion of validation set
#' @param levels Threshold of determine a feature to be categorical and qualified for stratification
#' @param seed Random seeds for reproducibility
#' @param DT If the function returns a list of data.tables or data.frames
#'
#' @return A \code{list} of \code{data.table} or \code{data.frame}
#' @examples
#' data(mtcars)
#' res <- splitDataToTrainTestStratified(mtcars, names(mtcars), 0.7, 0.3, 15, 999, T)
#' res$trainData
#' res$testData
#' res$evalData
#'
#' @export
splitDataToTrainTestStratified <- function(x, strataCols = NULL, trainProportion, evalProportion = NULL,
                                           levels = 50, seed = 999, DT = T) {
  # for reproducibility
  set.seed(seed)
  library(data.table)
  setDT(x)
  # main
  if(is.null(strataCols)){
    message('No strata features provided, will be proceeding with normal sampling')
    res <- splitDataToTrainTestDataFrame(x, trainProportion, evalProportion, seed, F)
  }else{
    # check if there is wrong feature names
    if(!all(strataCols%in%names(x))){
      stop("some variables provided not in data set")
    }

    # filter possible continuous features
    filterCols <- unlist(lapply(strataCols, function(c) nrow(unique(x[,c, with = F]))))
    filterCols <- strataCols[filterCols<=levels]
    strataCols <- strataCols[strataCols%in%filterCols]
    if(length(strataCols)<1) stop("no variable left after removing continuous varirables, please update and rerun...")

    # start sampling for train / test
    if(trainProportion < 1 & trainProportion > 0){
      df.table <- x[, .N, by = strataCols]
      n <- df.table[, ss := round(N * trainProportion, digits = 0)]
      setkeyv(x, strataCols)
      setkeyv(n, strataCols)

      x[, .RNID := sequence(nrow(x))]
      trainData <- x[x[n, list(.RNID = sample(.RNID, ss, replace = F)), by = .EACHI]$`.RNID`]
      testData <- x[!.RNID %in% trainData$`.RNID`]
      trainData[, .RNID := NULL]
      testData[, .RNID := NULL]
    }else{
      trainData <- x
      testData <- NULL
    }

    # start sampling for validation
    if(!is.null(evalProportion)){
      x <- trainData
      df.table <- x[, .N, by = strataCols]
      n <- df.table[, ss := round(N * (1-evalProportion), digits = 0)]
      setkeyv(x, strataCols)
      setkeyv(n, strataCols)

      x[, .RNID := sequence(nrow(x))]
      trainData <- x[x[n, list(.RNID = sample(.RNID, ss, replace = F)), by = .EACHI]$`.RNID`]
      evalData <- x[!.RNID %in% trainData$`.RNID`]
      trainData[, .RNID := NULL]
      evalData[, .RNID := NULL]
    }else{
      evalData <- NULL
    }

    # put everthing into a list to be returned
    if(DT){
      res <- list(trainData = trainData,
                  testData = testData,
                  evalData = evalData
                  )
    }else{
      res <- list(trainData = as.data.frame(trainData),
                  testData = as.data.frame(testData),
                  evalData = as.data.frame(evalData)
                  )
    }
  }
  gc()
  return(res)
}
