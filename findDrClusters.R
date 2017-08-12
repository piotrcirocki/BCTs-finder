#dodać wiersze jako pojedyncze wpisy - dodać po prostu ID
#wybierać wiersze, po trzech atrybutach

#sink(stdout(), type="message")
library("Rsamtools")
getwd()
setwd("/Users/piotrcirocki/Documents/github/BCT")
source("setConfiguration.R")
source("setAdditionalMethods.R")

#setwd("/Users/piotrcirocki/Downloads")
#library("formatR")
library("dplyr")
#tidy_dir(path = ".", recursive = FALSE)

#> a <- list(0)
#> a <- list(a, 5)
#> a <- list(a, 6)

tresholdValue = 5  #wartosc oznaczająca, próg przy jakim wartość jest zaliczana jako grupa
countBufferValue = 10  # wartość do jakiej może wystąpić bład w nieprawidlowych readach
numberOfBases = 10000 #sprawdzic w warunku czy roznica pozycji < 10000 
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
setwd("/Users/piotrcirocki/Downloads")
mydf = readRDS(file="data.Rda")

run <- function(tresholdValue , countBufferValue, bamFilePath, startIndex, endIndex, numberOfBases) {
  #loading configuration
  methodHelper <- HelperClass()
  i = 0
  
  #if (i == 1) {
  print(Sys.time())
  row = mydf[startIndex, ]
  methodHelper$setAddNewRow(row)
  #}
  
  for (i in (startIndex+1):endIndex) {
    row = mydf[i, ]
    # if 1st row
    
    #if 2nd and further rows
    # if (i != 1) {
    if(i %% 1000 ==0 ){
      print(i)  
      print(Sys.time())
    }
    
    #find element that already exists in temporary DataFrame
    relevantElement = methodHelper$getAndFindRelevantElements(as.character(row$rname), as.character(row$rdir), as.character(row$mrnm), as.character(row$pos), numberOfBases)
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
    #}
  }
  
  print(methodHelper$filterWithTreshold(tresholdValue))
  
  saveRDS(methodHelper$filterWithTreshold(tresholdValue), file = "genomeData.Rda")
  #saveRDS(methodHelper$getDfR(), file = "genomeData.Rda")
  #write.csv(methodHelper$getDfR(), file = "genome.csv")
}

debug(run)
startIndex = 1
endIndex = 100
run(tresholdValue , countBufferValue , bamFilePath, startIndex, endIndex, numberOfBases)
#todo dodać nowe informacje do nowych rekordów, nie stare.
#odsiać tylko te z przekroczonym  treshold 
#zapisać do pliku


