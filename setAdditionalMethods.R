HelperClass <- function() {
  
  thisEnv <- environment()
  
  dfR <- data.frame(chr = character(), ID = integer(), pos = character(), flag = character(), mrnm = character(), mpos = character(), 
                    countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE)
  
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
               #newRow$pos <- list(as.character(mydfRow$pos))
               newRow$pos = as.character(row$pos)
               #newRow$flag <- list(as.character(mydfRow$flag))
               newRow$flag = as.character(row$flag)
               newRow$mrnm = as.character(mydfRow$mrnm)
               newRow$mpos = as.character(row$mpos)
               #newRow$mpos <- list(as.character(mydfRow$mpos))
               newRow$countBuffer = 0
               newRow$countBuffer = as.integer(newRow$countBuffer)
               newRow$treshold = mydfRow$treshold
               newRow$isRelevant = TRUE
               newRow$dir <- mydfRow$dir
               dfR =   dplyr::bind_rows(dfR,newRow)
               assign("dfR",dfR,thisEnv)
             }, getAndFindRelevantElements = function(chromName, findDir, mrnrmEl) {
               dfR %>% filter(isRelevant == TRUE, chr == chromName, dir == findDir , mrnm == mrnrmEl)   %>%  filter(row_number()==1 ) -> relevantElement
               
               #dfR %>% mutate_cond((isRelevant == TRUE & chr == chromName & dir == findDir & mrnm == mrnrmEl), treshold =  treshold +1) -> dfR %>%  filter(row_number()==1 ) -> relevantElement
               return(relevantElement)
             }, 
             setTreshold = function(chromName, findDir, mrnrmEl){
               dfR %>% mutate_cond((isRelevant == TRUE & chr == chromName & dir == findDir & mrnm == mrnrmEl), treshold =  treshold +1) -> dfR
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
               #newRow$treshold = as.integer(newRow$treshold)
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
               #dfR <- as.data.frame(dfR)
               assign("dfR",dfR,thisEnv)
             },
             setElementsToNonRelevant = function(countBufferValue){
               # oznaczenie rekordow , jako is Relevant - False
               #nie spełniają one już więcej warunku - bufor dla nich został przekroczony
               dfR %>% mutate_cond((countBuffer > countBufferValue & isRelevant == TRUE), isRelevant = FALSE) -> dfR 
               #dfR <- as.data.frame(dfR)
               assign("dfR",dfR,thisEnv)
             },
             filterWithTreshold = function(tresholdValue){
               dfR %>% filter(treshold >= tresholdValue)-> dfRFiltered
               return(dfRFiltered)
             },
             getDfR = function(){
               assign("MyDataFrame",dfR,thisEnv)
               return(get("MyDataFrame",thisEnv))
             }
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"HelperClass")
  return(me)
}
