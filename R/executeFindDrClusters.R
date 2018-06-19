#' Function looks for discordant reads with the same chromosome name, mate and direction in close distance and assigna them the same id.
#' @import dtplyr
#' @param configuredDf gets dataframe result of function getConfiguration
#' @param tresholdValue specifies how many similar elements should be in the sequence
#' @param countBufferValue defines how many mismatched reads can be from another sequence to start ignore current sequence
#' @param startIndex defines from which position start read configured dataframe
#' @param endIndex defines to which position start read configured data from
#' @param numberOfPositionBases defines the maximum length of sequence position from the first element in sequence
#' @param numberOfMrnmBases defines the maximum length of sequence mrnm from the first element in sequence
#' @param filteredChromomes defines character vector of given chromosomes
#' @return dataframe of sequences with the same id
#' @export
findDrClusters <- function(configuredDf, tresholdValue , countBufferValue, numberOfPositionBases, numberOfMrnmBases, startIndex = NULL, endIndex = NULL, filteredChromomes = NULL) {
  methodHelper <- HelperClass()
  i = 0

  oldw <- getOption("warn")
  options(warn = -1)

  #[your "silenced" code]

  options(warn = oldw)

  #vc <- c('a', 'c')
  #dt[is.element(dt$fct, vc),]

  print(Sys.time())




  if(!is.null(filteredChromomes)){

    configuredDf <- configuredDf[is.element(configuredDf$rname, filteredChromomes),]

  }

  if(!is.null(startIndex) && !is.null(endIndex)){

    row = configuredDf[startIndex, ]
    methodHelper$setAddNewRow(row)
    configuredDf = configuredDf[(startIndex+1):endIndex,]

  }
  else {
    row = configuredDf[1, ]
    methodHelper$setAddNewRow(row)
    configuredDf = configuredDf[2:nrow(configuredDf),]
  }





  configuredDf = as.data.frame(configuredDf)
  #for (i in (startIndex+1):endIndex) {
  #row = configuredDf[i, ]
  configuredDf %>% rowwise() %>% do({
    # if 1st row
    row = as.data.frame(.)

    #if 2nd and further rows
    #if(i %% 100000 ==0 ){
    # print(i)
    #  print(Sys.time())
    #}

    #find element that already exists in temporary DataFrame
    relevantElement = methodHelper$getAndFindRelevantElements(as.character(row$rname), as.character(row$rdir), as.character(row$mrnm), as.character(row$pos), as.character(row$mpos), numberOfPositionBases,numberOfMrnmBases)

    #if any already in DataFrame
    if (nrow(relevantElement) > 0) {

      methodHelper$setAddRowToExistingElement(relevantElement, relevantElement$ID, row)
      methodHelper$setTreshold(relevantElement$ID)
      methodHelper$setCountForWrongRecords(relevantElement$ID)
      methodHelper$setElementsToNonRelevant(countBufferValue)
      methodHelper$moveElementsToOutputDataFrame()
    }
    else {
      methodHelper$setAddNewRow(row)
    }
    row
    #}
  })
  options(warn = oldw)
  return(methodHelper$filterWithTreshold(tresholdValue))
}
