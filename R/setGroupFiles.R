# ten sam chr,pozycje +5000 par zasad i ten sam chr w parze i pozycje w parze + 5000 par zasad
#grupowanie
#1. znaleźć grupy podobnych odczytów
#a  ten sam chrname, ten sam mrnm (para chromosomu), ten sam kierunek + 5000 par zasad - pos
#b najdlaszy revers z grupy i najwczesniejszy forward

#wziąc opcję -> read jeden wcześniej dla reversa, jeden pozniej dla forwarda



#Dla każdego elementu:

# 1 dodaj nową kolumnę - "czy już znaleziona". OK
# 2 Jeśli nieznaleziona to:
#    Jeśli forward:
#    znajdz taki sam chr , mrnm, R, ostatni revers z zakresu - 2000 sekwencji w starym DataFrame


#znajdz ostatnią pozycję Revers w ID (jeśli nie będą sortowane, to sortowanie po pozycji)
#    Jeśli revers:
#    znajdz taki sam chr , mrnm, F, pierwszy forward z zakresu + 2000 sekwencji w starym DataFrame




# 3. oznacz w starym dataFrame jako już juz znalezione
# 4. dodaj do nowego dataFrame"

#nazwa grupy
#chr1  mrnm1 chr2 mrnm2 #R1 #F1 #pos #pos + userValue

#zwróc pos dla forwarda jako pos + zmienna uzytkownika, domyślnie 101
#przedotatni forward drugi revers
#



GroupFilesManager <- function() {

  thisEnv <- environment()

  dfRame <- data.frame(chr = character(), ID = integer(), pos = character(), flag = character(), mrnm = character(), mpos = character(),
                       countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE, absolutePosition = integer())


  dfRameNewWithGroups <- data.frame(chrSymbol1 = character(), mrnm1 = character(),dir1 = character(), chr1Pos = character(), chrSymbol2 = character(), mrnm2 = character(),
                                    dir2 = character(),chr2Pos = character(),
                                    stringsAsFactors = FALSE, treshold = integer())


  mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
    condition <- eval(substitute(condition), .data, envir)
    .data[condition, ] <- .data[condition, ] %>% mutate(...)
    .data
  }

  me <- list(thisEnv = thisEnv,
             getdfRame = function(){
               assign("dfRame",dfRame,thisEnv)
               return(get("dfRame",thisEnv))
             },
             setDataFrame = function(dataFrame){
               assign("dfRame",dataFrame,thisEnv)
               return(dfRame)
             },
             getDataFrame = function(){
               #dfRame <- read.table("genome.csv", header = TRUE)
               dfRame = readRDS(file="genomeData.Rda")
               assign("dfRame",dfRame,thisEnv)
               return(dfRame)
             },
             setNewColumn = function(){

               dfRame =  dplyr::mutate(dfRame, visited = FALSE)
               assign("dfRame",dfRame,thisEnv)
               #return(dfRame)
             },

             findGroupsInChromosonmes = function(userDefinedPostion, positionInPairDistance, sameDirection = FALSE, distanceBetweenElements = 1){



               for (i in 1: nrow(dfRame)) {
                 actDf = dfRame[i,]
                 # dfRElement =
                 if(actDf$visited == TRUE){
                   next
                 }

                 if(actDf$dir == "F"){

                   if(sameDirection == TRUE){
                     direction = "F"
                   }
                   else {
                     direction = "R"
                   }
                   subsetDf = subset(dfRame, ID == actDf$ID)
                   actDf = subsetDf[which.min(subsetDf$pos)+distanceBetweenElements,]
                   if(nrow(actDf)== 0 && nrow(subsetDf) >0 ){
                     actDf = subsetDf[1,]
                   }
                   dfRame %>% filter( chr == actDf$chr, dir == direction , mrnm == actDf$mrnm , (   as.integer(actDf$pos) - as.integer(dfRame$pos)   )  <= positionInPairDistance, (   as.integer(actDf$pos) - as.integer(dfRame$pos)   )  > 0)-> relevantElementGroup
                  # relevantElements = dfRame[which(dfRame$ID ==relevantElementGroup$ID ), ]
                   element  <- relevantElementGroup[which.max(relevantElementGroup$pos) - distanceBetweenElements,]
                   if(nrow(element)==0 || is.na(element$chr) ){
                     next
                   }
                   newRow <- list()
                   newRow$chrSymbol1 = element$chr
                   newRow$mrnm1 = element$mrnm
                   newRow$dir1 = element$dir  # dir1 zawsze niech będzie revers
                   newRow$chr1Pos = element$pos


                   newRow$chrSymbol2 = actDf$chr
                   newRow$mrnm2 = actDf$mrnm
                   newRow$dir2 = actDf$dir
                   newRow$chr2Pos = as.character(as.integer(actDf$pos) + userDefinedPostion)
                   newRow$treshold = element$treshold
                   dfRame %>% mutate_cond((ID == actDf$ID), visited = TRUE) -> dfRame
                   dfRame %>% mutate_cond((ID == element$ID), visited = TRUE) -> dfRame


                   dfRameNewWithGroups =   dplyr::bind_rows(dfRameNewWithGroups,newRow)
                   assign("dfRameNewWithGroups",dfRameNewWithGroups,thisEnv)
                   assign("dfRame",dfRame,thisEnv)

                 }

                 if(actDf$dir == "R"){
                   if(sameDirection == TRUE){
                     direction = "R"
                   }
                   else {
                     direction = "F"
                   }
                   subsetDf = subset(dfRame, ID == actDf$ID)

                   actDf = subsetDf[which.max(subsetDf$pos)-distanceBetweenElements,]
                  if(nrow(actDf)== 0 && nrow(subsetDf) >0 ){
                    actDf = subsetDf[1,]
                  }
                   dfRame %>% filter( chr == actDf$chr, dir == direction , mrnm == actDf$mrnm , ( as.integer(dfRame$pos) - as.integer(actDf$pos) )  <= positionInPairDistance  ,( as.integer(dfRame$pos) - as.integer(actDf$pos) > 0) ) -> relevantElementGroup
                   #relevantElements = dfRame[which(dfRame$ID ==relevantElementGroup$ID ), ]
                   element  <- relevantElementGroup[which.min(relevantElementGroup$pos)+ distanceBetweenElements,]
                   if(nrow(element)==0 || is.na(element$chr) ){
                     next
                   }
                   newRow <- list()
                   newRow$chrSymbol1 = actDf$chr
                   newRow$mrnm1 = actDf$mrnm
                   newRow$dir1 = actDf$dir  # dir1 zawsze niech będzie revers
                   newRow$chr1Pos = actDf$pos

                   newRow$chr2Pos = as.character(as.integer(element$pos) + userDefinedPostion)
                   newRow$dir2 = element$dir
                   newRow$chrSymbol2 = element$chr
                   newRow$mrnm2 = element$mrnm
                   newRow$treshold = element$treshold

                   dfRame %>% mutate_cond((ID == actDf$ID), visited = TRUE) -> dfRame
                   dfRame %>% mutate_cond((ID == element$ID), visited = TRUE) -> dfRame
                   dfRameNewWithGroups =   dplyr::bind_rows(dfRameNewWithGroups,newRow)
                   assign("dfRameNewWithGroups",dfRameNewWithGroups,thisEnv)
                   assign("dfRame",dfRame,thisEnv)
                 }

               }

             },

             getDataFrameWithGroups = function(){
               assign("dfRameNewWithGroups",dfRameNewWithGroups,thisEnv)
               return(get("dfRameNewWithGroups",thisEnv))

             }

  )

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  ## Set the name for the class
  class(me) <- append(class(me),"GroupFilesManager")
  return(me)
}
