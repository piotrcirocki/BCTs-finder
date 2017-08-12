ConfigurationInitializer <- function() {
  
  thisEnv <- environment()
  
  me <- list(thisEnv = thisEnv, setConfiguration = function(value) {
    what <- c("rname", "pos", "qwidth", "seq", "isize", "flag", "mrnm", "mpos")
    
    param <- ScanBamParam(what = what, flag = scanBamFlag(isProperPair = F, isUnmappedQuery = FALSE, hasUnmappedMate = FALSE))
    
    bam <- scanBam(value, param = param)
    
    .unlist <- function(x) {
      x1 <- x[[1L]]
      if (is.factor(x1)) {
        structure(unlist(x), class = "factor", levels = levels(x1))
      } else {
        do.call(c, x)
      }
    }
    bam <- unname(bam)  # names not useful in unlisted result
    elts <- setNames(bamWhat(param), bamWhat(param))
    lst <- lapply(elts, function(elt) .unlist(lapply(bam, "[[", elt)))
    df <- do.call("DataFrame", lst)
    
    df["rdir"] <- sapply(df$flag, function(x) {
      index <- "N"
      if (bitwAnd(x, 64) != 0) {
        index <- "F"
      } else if (bitwAnd(x, 128) != 0) {
        index <- "R"
      } else index <- "N"
      return(index)
    })
    mydf <- df[!df$rname == "chrM", ]
    mydf <- mydf[!mydf$mrnm == "chrM", ]
    
    mydf$ID <- seq.int(nrow(mydf))
    
    return(assign("myDataframe", mydf, thisEnv))
  }, getConfiguration = function() {
    return(get("myDataframe", thisEnv))
  })
  
  assign("this", me, envir = thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me), "ConfigurationInitializer")
  return(me)
}
