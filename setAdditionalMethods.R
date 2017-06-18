HelperClass <- function() {
  
  thisEnv <- environment()
  
  dfR <- data.frame(chr = factor(), ID = integer(), pos = character(), flag = character(), mrnm = factor(), mpos = character(), 
                    countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE)
  
  
  me <- list(thisEnv = thisEnv, 
        setAddRowToExistingElement = function(mydfRow, searchID) {
        existingRow <- dfR[which(dfR$ID == searchID), ]
        existingRow$pos <- paste(mydfRow$pos, existingRow$pos, collapse = ",")
        existingRow$flag <- paste(mydfRow$flag, dfR[1, ]$flag, collapse = ",")
        existingRow$mpos <- paste(mydfRow$mpos, dfR[1, ]$mpos, collapse = ",")
        existingRow$treshold = dfR[searchID, ]$treshold + 1
        existingRow$countBuffer = 0
        dfR[which(dfR$ID == searchID), ] = existingRow
        assign("dfR",dfR,thisEnv)
    }, getAndFindRelevantElements = function(chromName, findDir, mrnrmEl) {
        #relevantElement <- dfR[(dfR$isRelevant != FALSE & dfR$chr == chromName & dfR$rdir == dir), , drop = FALSE]
       dfR %>% filter(isRelevant == TRUE, chr == chromName, dir == findDir , mrnm == mrnrmEl) -> relevantElement
      return(relevantElement)
    }, setAddNewRow = function(mydfRow) {
        newRow <- list()
        newRow$chr = mydfRow$rname
        newRow$ID = mydfRow$ID
        newRow$pos <- as.character(mydfRow$pos)
        newRow$flag <- as.character(mydfRow$flag)
        newRow$mrnm = mydfRow$mrnm
        newRow$mpos <- as.character(mydfRow$mpos)
        newRow$dir <- mydfRow$rdir
        newRow$countBuffer = 0
        newRow$treshold = 1
        newRow$isRelevant = TRUE
        #newRow[, c(3, 4, 6)] <- sapply(newRow[, c(3, 4, 6)], as.character)
        dfR =   dplyr::bind_rows(dfR,newRow)
        assign("dfR",dfR,thisEnv)
        # dfR = rbind(dfR, newRow)
        #
        
        },getDataFrame = function(){
      return(get("dfR",thisEnv))
    },
    # naliczanie do bufora dla rekordów, które się nie pojawiły - nakladanie kary
    setCountForWrongRecords = function(searchID){
      dfR %>% mutate(countBuffer = replace(countBuffer, (ID != searchID & isRelevant == TRUE) , countBuffer +1)) %>% as.data.frame() -> dfR
      assign("dfR",dfR,thisEnv)
      #records = dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]
     # records$countBuffer = records$countBuffer + 1
      #dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]$countBuffer = dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]$countBuffer + 1
    },
    setElementsToNonRelevant = function(countBufferValue){
      # oznaczenie rekordow , jako is Relevant - False
      #nie spełniają one już więcej warunku - bufor dla nich został przekroczony
      dfR %>% mutate(isRelevant = replace(isRelevant, (countBuffer > countBufferValue & isRelevant == TRUE), FALSE)) %>% as.data.frame() -> dfR
      #if (nrow(dfR[which(dfR$countBuffer > countBufferValue & dfR$isRelevant == TRUE), ]) > 0) {
      #}
      assign("dfR",dfR,thisEnv)
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
