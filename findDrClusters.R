#dodać wiersze jako pojedyncze wpisy - dodać po prostu ID
#wybierać wiersze, po trzech atrybutach

sink(stdout(), type="message")
library("Rsamtools")
getwd()
setwd("/Users/piotrcirocki/Documents/github/BCT")
source("setConfiguration.R")
source("setAdditionalMethods.R")

setwd("/Users/piotrcirocki/Downloads")
#library("formatR")
library("dplyr")
#tidy_dir(path = ".", recursive = FALSE)

#> a <- list(0)
#> a <- list(a, 5)
#> a <- list(a, 6)

tresholdValue = 3  #wartosc oznaczająca, próg przy jakim wartość jest zaliczana jako grupa
countBufferValue = 3  # wartość do jakiej może wystąpić bład w nieprawidlowych readach
bamFilePath = "/Users/piotrcirocki/Downloads/Sample_R_1510_MP5_1791_15_S8.bwa.bam"
startSearachIndex = 1
endIndex = 100

browser()
#debug(run)
#run
startIndex = 1
endIndex = 10
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
            #if any in DataFrame
            if (nrow(relevantElement) > 0) {
                methodHelper$setAddRowToExistingElement(relevantElement, relevantElement$ID)
                methodHelper$setCountForWrongRecords(relevantElement$ID)
                methodHelper$setElementsToNonRelevant(countBufferValue)
                }
            else {
              methodHelper$setAddNewRow(row)
            }
        }
    }
    print(methodHelper$getDfR())
}

debug(run)

run(tresholdValue , countBufferValue , bamFilePath, startIndex, endIndex)

