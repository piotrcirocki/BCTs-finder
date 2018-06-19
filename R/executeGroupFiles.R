#' Function finds the groups of similar reads.
#' @param dataFrame gets the datframe of similar sequences
#' @return dataframe with groups
#' @return positionInPairDistance - distance between groups
#' @return sameDirection - decides if reads should be in the same diretion or not
#' @return distanceBetweenElements determines how many elements is between the center of a pair
#' @export
groupFiles = function(dataFrame, positionInPairDistance, sameDirection, distanceBetweenElements){
  groupManager <- GroupFilesManager()
  groupManager$setDataFrame(dataFrame)
  groupManager$setNewColumn()
  groupManager$findGroupsInChromosonmes(0, positionInPairDistance, sameDirection, distanceBetweenElements)
  groupManager$getDataFrameWithGroups()
}
