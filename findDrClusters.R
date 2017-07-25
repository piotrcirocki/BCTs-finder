#dodać wiersze jako pojedyncze wpisy - dodać po prostu ID
#wybierać wiersze, po trzech atrybutach

#sink(stdout(), type="message")
library("Rsamtools")
getwd()
setwd("/home/users/xpcirock/Desktop")
source("setConfiguration.R")
source("setAdditionalMethods.R")

#setwd("/Users/piotrcirocki/Downloads")
#library("formatR")
library("dplyr")
#tidy_dir(path = ".", recursive = FALSE)

#> a <- list(0)
#> a <- list(a, 5)
#> a <- list(a, 6)

tresholdValue = 2  #wartosc oznaczająca, próg przy jakim wartość jest zaliczana jako grupa
countBufferValue = 10  # wartość do jakiej może wystąpić bład w nieprawidlowych readach
bamFilePath = "/Users/piotrcirocki/Downloads/Sample_R_1510_MP5_1791_15_S8.bwa.bam"
startSearachIndex = 1
endIndex = 100

browser()
#debuggingState(on=FALSE)
#debug(run)
#run

#configuration <- ConfigurationInitializer()
#configuration$setConfiguration(bamFilePath)
#configuration$getConfiguration()

mydf = readRDS(file="data.Rda")

run <- function(tresholdValue , countBufferValue, bamFilePath, startIndex, endIndex) {
  #loading configuration
  methodHelper <- HelperClass()
  i = 0
  
  for (i in startIndex:endIndex) {
    row = mydf[i, ]
    # if 1st row
    if (i == 1) {
      methodHelper$setAddNewRow(row)
    }
    #if 2nd and further rows
    if (i != 1) {
      #find element that already exists in temporary DataFrame
      relevantElement = methodHelper$getAndFindRelevantElements(as.character(row$rname), as.character(row$rdir), as.character(row$mrnm))
      methodHelper$setTreshold(as.character(row$rname), as.character(row$rdir), as.character(row$mrnm))
      #if any in DataFrame
      if (nrow(relevantElement) > 0) {
        
        methodHelper$setAddRowToExistingElement(relevantElement, relevantElement$ID, row)
        methodHelper$setCountForWrongRecords(relevantElement$ID)
        methodHelper$setElementsToNonRelevant(countBufferValue)
      }
      else {
        methodHelper$setAddNewRow(row)
      }
    }
  }
  
  print(methodHelper$filterWithTreshold(tresholdValue))
  #saveRDS(methodHelper$getDfR(), file = "genomeData.Rda")
  write.csv(methodHelper$getDfR(), file = "genome.csv")
}

debug(run)
startIndex = 1
endIndex = 20
run(tresholdValue , countBufferValue , bamFilePath, startIndex, endIndex)
#todo dodać nowe informacje do nowych rekordów, nie stare.
#odsiać tylko te z przekroczonym  treshold 
#zapisać do pliku


