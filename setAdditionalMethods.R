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
          
          newRow <- list()
          newRow$chr = as.character(mydfRow$chr)
          newRow$ID = searchID
          newRow$pos <- list(as.character(mydfRow$pos))
          newRow$flag <- list(as.character(mydfRow$flag))
          newRow$mrnm = as.character(mydfRow$mrnm)
          newRow$mpos <- list(as.character(mydfRow$mpos))
          newRow$countBuffer = 0
          newRow$countBuffer = as.integer(newRow$countBuffer)
          newRow$treshold = 1
          newRow$treshold = as.integer(newRow$treshold)
          newRow$isRelevant = TRUE
          newRow$dir <- mydfRow$dir
          dfR =   dplyr::bind_rows(dfR,newRow)
          assign("dfR",dfR,thisEnv)
    }, getAndFindRelevantElements = function(chromName, findDir, mrnrmEl) {
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
