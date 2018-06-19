#'Function prepares bam file to further processing
#' @param  file bam file
#' @return dataframe with configured columns
#' @export
prepareConfiguration = function(file){
  #source("setConfiguration.R")
  configuration <- ConfigurationInitializer()
  configuration$setConfiguration(file)
  configuration$getConfiguration()
}
