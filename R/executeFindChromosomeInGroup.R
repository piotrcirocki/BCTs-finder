#' Function finds chromosome in group.
#' @param dataFrame gets the datframe of group dataframe
#' @param  chromosomeNumberOne number of reverse Chromosome given by the user
#' @param  chromosomeNumberTwo number of forward Chromosome given by the user
#' @return result with group of chromosome
#' @export
findChromosomeInGroup = function(dataFrame, chromosomeNumberOne, chromosomeNumberTwo){

  if(!is.null(chromosomeNumberOne) && !is.null(chromosomeNumberTwo)){
    dataFrame1  <- dataFrame[which(dataFrame$chrSymbol1 == chromosomeNumberOne && dataFrame$chrSymbol2 == chromosomeNumberTwo), ]

    dataFrame2  <- dataFrame[which(dataFrame$chrSymbol1 == chromosomeNumberTwo && dataFrame$chrSymbol2 == chromosomeNumberOne), ]
    result = merge(dataFrame1, dataFrame2)
    return(result)
  }

  if(is.null(chromosomeNumberForward) || is.null(chromosomeNumberReverse) || is.null(dataFrame) ){
    print("Error: Required parameters not set.")
  }
}
