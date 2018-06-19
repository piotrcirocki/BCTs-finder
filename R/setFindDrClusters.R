#' @import dplyr
HelperClass <- function() {

  thisEnv <- environment()

  dfR <- data.frame(chr = character(), ID = integer(), pos = character(), flag = character(), mrnm = character(), mpos = character(),
                    countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE, absolutePosition = integer(), absoluteMrnmPosition = integer())
  dfROutput <- data.frame(chr = character(), ID = integer(), pos = character(), flag = character(), mrnm = character(), mpos = character(),
                          countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE, absolutePosition = integer(), absoluteMrnmPosition = integer())
  #setkey(dfR,ID)
  #setkey(dfR, isRelevant)
  #setkey(dfR,chr)

  mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
  }
  me <- list(thisEnv = thisEnv,
             setAddRowToExistingElement = function(mydfRow, searchID, row) {

               newRow <- list()
               newRow$chr = as.character(mydfRow$chr)
               newRow$ID = searchID
               newRow$pos = as.character(row$pos)
               newRow$flag = as.character(row$flag)
               newRow$mrnm = as.character(mydfRow$mrnm)
               newRow$mpos = as.character(row$mpos)
               newRow$countBuffer = 0
               newRow$countBuffer = as.integer(newRow$countBuffer)
               newRow$treshold = mydfRow$treshold
               newRow$absolutePosition = mydfRow$absolutePosition
               newRow$absoluteMrnmPosition = mydfRow$absoluteMrnmPosition
               newRow$isRelevant = TRUE
               newRow$dir <- mydfRow$dir
               dfR =   dplyr::bind_rows(dfR,newRow)
               assign("dfR",dfR,thisEnv)
             },
             #sprawdzic w warunku czy roznica pozycji < 10000
             #chodzi o wartos bezwzgledna
             # actualPosition to pozycja wyszukanego rekordu w mydf
             # absolutePosition absolute posiotion to wyszukiwanie pozycji absolutniej w moim dfR

             getAndFindRelevantElements = function(chromName, findDir, mrnrmEl, actualPosition,actualMrnmPosition, numberOfPositionBases, numberOfMrnmBases) {
               #'@import dplyr
               dfR %>% filter(isRelevant == TRUE, chr == chromName, dir == findDir , mrnm == mrnrmEl,  (absolutePosition - as.integer(actualPosition)) <= numberOfPositionBases, abs((absoluteMrnmPosition - as.integer(actualMrnmPosition))) <= numberOfMrnmBases)  %>%  filter(row_number()==1 ) -> relevantElement

               return(relevantElement)
             },
             setTreshold = function(searchID){
               dfR %>% mutate_cond((ID == searchID & isRelevant == TRUE), treshold =  treshold +1) -> dfR
               assign("dfR",dfR,thisEnv)
             },

             setAddNewRow = function(mydfRow) {
               newRow <- list()
               newRow$chr = as.character(mydfRow$rname)
               newRow$ID = mydfRow$ID
               newRow$pos <- as.character(mydfRow$pos)
               newRow$flag <- as.character(mydfRow$flag)
               newRow$mrnm = as.character(mydfRow$mrnm)
               newRow$mpos <- as.character(mydfRow$mpos)
               newRow$countBuffer = 0
               newRow$countBuffer = as.integer(newRow$countBuffer)
               newRow$treshold = 1
               newRow$absolutePosition = mydfRow$pos
               newRow$absoluteMrnmPosition = mydfRow$mpos
               newRow$isRelevant = TRUE
               newRow$dir <- mydfRow$rdir
               dfR =   dplyr::bind_rows(dfR,newRow)
               assign("dfR",dfR,thisEnv)
             },
             getDataFrame = function(){
               return(get("dfR",thisEnv))
             },
             # naliczanie do bufora dla rekordów, które się nie pojawiły - nakladanie kary
             setCountForWrongRecords = function(searchID){
               dfR %>% mutate_cond((ID != searchID & isRelevant == TRUE), countBuffer = countBuffer +1) -> dfR
               assign("dfR",dfR,thisEnv)
             },
             setElementsToNonRelevant = function(countBufferValue){
               # oznaczenie rekordow , jako is Relevant - False
               # nie spełniają one już więcej warunku - bufor dla nich został przekroczony
               dfR %>% mutate_cond((countBuffer > countBufferValue & isRelevant == TRUE), isRelevant = FALSE) -> dfR
               assign("dfR",dfR,thisEnv)
             },
             moveElementsToOutputDataFrame = function(){
               dfR %>% filter(isRelevant == FALSE) -> elementsToMove
               if(nrow(elementsToMove)!=0){
                 dfROutput =dplyr::bind_rows(dfROutput,elementsToMove)
                 dfR =  anti_join(dfR, elementsToMove, by="isRelevant")
                 assign("dfR",dfR,thisEnv)
                 assign("dfROutput",dfROutput,thisEnv)
               }
             },
             filterWithTreshold = function(tresholdValue){
               oldw <- getOption("warn")
               options(warn = -1)
               dfROutput %>% filter(treshold >= tresholdValue)-> dfRFiltered
               dfRFiltered %>% group_by(ID) %>% filter(n()>1) -> dfRFilteredOutput
               c = as.data.frame(dfRFilteredOutput)
               options(warn = oldw)
               return(c)
             },
             getDfR = function(){
               assign("myDataFrame",dfROutput,thisEnv)
               return(get("myDataFrame",thisEnv))
             }
  )

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)

  ## Set the name for the class
  class(me) <- append(class(me),"HelperClass")
  return(me)
}
