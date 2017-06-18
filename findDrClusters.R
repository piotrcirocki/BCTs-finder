# TODO function search for discordant reads clusters

# input - file with DRs output - data frame with DR clusters source('http://www.bioconductor.org/biocLite.R')
#'wziac tylko discordant ready'
# biocLite(c('Rsamtools'))

# setwd('/Users/piotrcirocki/Downloads') getwd() browseVignettes('Rsamtools') library('Rsamtools') ?which <-
# RangesList(seq1=IRanges(1000, 2000), seq2=IRanges(c(100, 1000), c(1000, 2000))) what <- c('rname', 'strand',
# 'pos', 'qwidth', 'seq')

# param <- ScanBamParam(which=which, what=what) param <- ScanBamParam(which=which, what=what) bamFile <-
# system.file('extdata', 'ex1.bam', package='Rsamtools') Sample_R_1510_MP5_1791_15_S8.bwa.bam bamFile <-
# system.file('Sample_R_1510_MP5_1791_15_S8.bwa.bam') bam <- scanBam(bamFile, param=param) bam

# bamFile

# findDrClusters <- function(){

# }

# ?system.file

# treshold - how many reads means that reads are in the same area

# chr rdir po ost ilosc odczytow poz mate chr mate

#library("formatR")
#tidy_dir(path = ".", recursive = FALSE)
#Sys.setenv(LANG = "en")

library("Rsamtools")
getwd()
setwd("/Users/piotrcirocki/Documents/github/BCT")
source("setConfiguration.R")
source("setAdditionalMethods.R")

setwd("/Users/piotrcirocki/Downloads")
library("formatR")
library("dplyr")
tidy_dir(path = ".", recursive = FALSE)


tresholdValue = 3  #wartosc oznaczająca, próg przy jakim wartość jest zaliczana jako grupa
countBufferValue = 3  # wartość do jakiej może wystąpić bład w nieprawidlowych readach
bamFilePath = "/Users/piotrcirocki/Downloads/Sample_R_1510_MP5_1791_15_S8.bwa.bam"


browser()
#debug(run)
#run
configuration <- ConfigurationInitializer()
configuration$setConfiguration("/Users/piotrcirocki/Downloads/Sample_R_1510_MP5_1791_15_S8.bwa.bam")
mydf = configuration$getConfiguration()

#main loop algorithm
run <- function(tresholdValue , countBufferValue, bamFilePath) {
    #loading configuration
   
    methodHelper <- HelperClass()
    i = 0
    
    for (i in 1:10) {
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
            else{
              methodHelper$setAddNewRow(row)
            }
        }
    }
    
    print(methodHelper$getDfR())
}

debug(run)

run(tresholdValue , countBufferValue , bamFilePath)

