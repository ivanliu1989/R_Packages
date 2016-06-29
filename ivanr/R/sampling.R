# x: input data.frame
# strataCols: features that will be used for stratified sampling
# trainProportion: % of data to be training set
# evalProportion:  % of data to be validation set
# levels: threshold of identifing categorical/continuous features
# seed: as the name
# verbose: as the name

splitDataToTrainTestStratified <- function(x, strataCols = NULL, trainProportion, evalProportion = NULL, levels = 50, seed = 999, verbose = F) {
  # for reproducibility
  set.seed(seed)
  library(data.table)
  setDT(x)
  # main
  if(is.null(strataCols)){
    message('No strata features provided, will be proceeding with normal sampling')
    res <- splitDataToTrainTestDataFrame(x, trainProportion, evalProportion, seed, verbose)
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
    df.table <- x[, .N, by = strataCols]
    n <- df.table[, ss := round(N * trainProportion, digits = 0)]
    setkeyv(x, strataCols)
    setkeyv(n, strataCols)
    
    x[, .RNID := sequence(nrow(x))]
    trainData <- x[x[n, list(.RNID = sample(.RNID, ss, replace = F)), by = .EACHI]$`.RNID`]
    testData <- x[!.RNID %in% trainData$`.RNID`]
    trainData[, .RNID := NULL]
    testData[, .RNID := NULL]
    
    # start sampling for validation
    if(!is.null(evalProportion)){
      x <- trainData
      df.table <- x[, .N, by = strataCols]
      n <- df.table[, ss := round(N * trainProportion, digits = 0)]
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
    res <- list(trainData = trainData, 
                testData = testData, 
                evalData = evalData)
  }
  gc()
  return(res)
}





