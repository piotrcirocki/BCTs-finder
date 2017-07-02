HelperClass <- function() {
  
  thisEnv <- environment()
  
  dfR <- data.frame(chr = character(), ID = integer(), pos = list(), flag = list(), mrnm = character(), mpos = list(), 
                    countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE)
 
   mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
  }
  
  
  me <- list(thisEnv = thisEnv, 
        setAddRowToExistingElement = function(mydfRow, searchID) {
        existingRow <- dfR[which(dfR$ID == searchID), ]
        #existingRow$pos[[length(existingRow$pos) + 1]] <- mydfRow$pos
        #existingRow$flag[[length(existingRow$flag) + 1]] <- mydfRow$flag
        #existingRow$flag[[length(existingRow$mpos) + 1]] <- mydfRow$mpos
        
        existingRow$pos <- paste(mydfRow$pos, dfR[searchID, ]$pos, collapse = ",")
        existingRow$flag <-paste(mydfRow$flag, dfR[searchID, ]$flag, collapse = ",")
        existingRow$mpos <- paste(mydfRow$mpos, dfR[searchID, ]$mpos, collapse = ",")
        
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
        newRow$chr = as.character(mydfRow$rname)
        newRow$ID = mydfRow$ID
        newRow$pos <- list(as.character(mydfRow$pos))
        newRow$flag <- list(as.character(mydfRow$flag))
        newRow$mrnm = as.character(mydfRow$mrnm)
        newRow$mpos <- list(as.character(mydfRow$mpos))
        newRow$countBuffer = 0
        newRow$countBuffer = as.integer(newRow$countBuffer)
        newRow$treshold = 1
        newRow$treshold = as.integer(newRow$treshold)
        newRow$isRelevant = TRUE
        newRow$dir <- mydfRow$rdir
        dfR =   dplyr::bind_rows(dfR,newRow)
        assign("dfR",dfR,thisEnv)
        # dfR = rbind(dfR, newRow)
        #
        
        },getDataFrame = function(){
      return(get("dfR",thisEnv))
    },
    # naliczanie do bufora dla rekordów, które się nie pojawiły - nakladanie kary
    setCountForWrongRecords = function(searchID){
      dfR %>% mutate_cond((ID != searchID & isRelevant == TRUE), countBuffer = countBuffer +1) -> dfR
      assign("dfR",dfR,thisEnv)
     
      # dfR %>% filter(ID != searchID & isRelevant == TRUE) %>% as.data.frame() -> temp
      #temp %>% mutate(countBuffer = countBuffer +1) %>% as.data.frame() -> dfR
      #assign("dfR",dfR,thisEnv)
      
      
      #records = dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]
     # records$countBuffer = records$countBuffer + 1
      #dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]$countBuffer = dfR[which(dfR$ID != searchID & dfR$isRelevant == TRUE), ]$countBuffer + 1
    },
    setElementsToNonRelevant = function(countBufferValue){
      # oznaczenie rekordow , jako is Relevant - False
      #nie spełniają one już więcej warunku - bufor dla nich został przekroczony
      dfR %>% mutate_cond((countBuffer > countBufferValue & isRelevant == TRUE), isRelevant = FALSE) -> dfR
      assign("dfR",dfR,thisEnv)
      #dfR %>% filter(countBuffer > countBufferValue & isRelevant == TRUE) %>% as.data.frame() -> temp
      #temp %>% mutate(isRelevant = FALSE) %>% as.data.frame() -> dfR
      
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
