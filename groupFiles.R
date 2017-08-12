#https://www.digitalocean.com/community/tutorials/how-to-use-sshfs-to-mount-remote-file-systems-over-ssh
#Hm ten sam chr,pozycje +5000 par zasad i ten sam chr w parze i pozycje w parze + 5000 par zasad
#grupowanie 
#1. znaleźć grupy podobnych odczytów
#a  ten sam chrname, ten sam mrnm (para chromosomu), ten sam kierunek + 5000 par zasad - pos
#b najdlaszy revers z grupy i najwczesniejszy forward


#wziąc opcję -> read jeden wcześniej dla reversa, jeden poniej dla forwarda



GroupFilesManager <- function() {
  
  thisEnv <- environment()
  
  dfRame <- data.frame(chr = character(), ID = integer(), pos = character(), flag = character(), mrnm = character(), mpos = character(), 
                       countBuffer = integer(), treshold = integer(), isRelevant = logical(), dir = character(), stringsAsFactors = FALSE, absolutePosition = integer())
  #chr1  mrnm1 chr2 mrnm2 #R1 #F1 #pos #pos + userValue 
  
  dfRameNewWithGroups <- data.frame(chrSymbol1 = character(), mrnm1 = character(), chrSymbol2 = character(), mrnm2 = character(), dir1 = character(),
                                    dir2 = character(), chr1Pos = character(),chr2Pos = character(),
                                    stringsAsFactors = FALSE)
  
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
             
             getDataFrame = function(){
               #dfRame <- read.table("genome.csv", header = TRUE)
               dfRame = readRDS(file="genomeData.Rda")
               assign("dfRame",dfRame,thisEnv)
               return(dfRame)
             },
             setNewColumn = funciton(){
               
               dplyr::mutate(dfRame, visited = FALSE)
             },
             
             findGroupsInChromosonmes = function(userDefinedPostion){
               actDf = dfRame[i,]
               for (i in 1: nrow(dfRame)) {
                 # dfRElement = 
                 if(actDf$visited == TRUE){
                   next
                 }
                 
                 if(actDf$dir == "F"){
                   #dfR %>% filter( chr == actDf$chr, dir == actDf$dir , mrnm == actDf$mrnm ,  )  %>%  filter(row_number()==1 ) -> relevantElement
                   dfRame %>% filter( chr == actDf$chr, dir == "R" , mrnm == actDf$mrnm , (actDf$pos - dfRame$pos)  <= 2000)  %>%  filter(row_number()==1 ) -> relevantElementGroup
                   element  <- relevantElementGroup[which.max(relevantElementGroup$pos),]
                   newRow <- list()
                   newRow$chrSymbol1 = element$chr
                   newRow$mrnm1 = element$mrnm
                   newRow$chrSymbol2 = actDf$chr
                   newRow$mrnm2 = actDf$mrnm
                   newRow$dir1 = element$dir  # dir1 zawsze niech będzie revers
                   newRow$dir2 = actDf$dir
                   newRow$chr1Pos = element$pos
                   newRow$chr2Pos = as.character(as.integer(actDf$pos + userDefinedPostion))
                   #ctrl + shift + m
                   dfRame %>% mutate_cond((ID == actDf$ID), visited = TRUE) -> dfRame
                   dfRame %>% mutate_cond((ID == element$ID), visited = TRUE) -> dfRame
                   
                   
                   dfRameNewWithGroups =   dplyr::bind_rows(dfRameNewWithGroups,newRow)
                   assign("dfRameNewWithGroups",dfRameNewWithGroups,thisEnv)
                   assign("dfRame",dfRame,thisEnv)
                   
                   # data.frame(chrSymbol1 = character(), mrnm1 = character(), chrSymbol2 = character(), mrnm2 = character(), dir1 = character(),
                   #           dir2 = character(), chr1Pos = character(),chr2Pos = character(),
                   #          stringsAsFactors = FALSE)
                   
                   
                   
                   #chr1  mrnm1 chr2 mrnm2 #R1 #F1 #pos #pos + userValue 
                   
                   # najbardziej wysuniety w lewo forward, ale pozycje juz mam  w kolumnie absolutePosition
                   # najbardzoej wysuniety w prawo revers
                   # zapisać i dodać uservualue do Forward
                   
                 }
                 
                 if(actDf$dir == "R"){
                   dfRame %>% filter( chr == actDf$chr, dir == "F" , mrnm == actDf$mrnm , ( dfRame$pos - actDf$pos )  <= 2000)  %>%  filter(row_number()==1 ) -> relevantElementGroup
                   element  <- relevantElementGroup[which.min(relevantElementGroup$pos),]
                   
                   newRow <- list()
                   newRow$chrSymbol1 = actDf$chr
                   newRow$mrnm1 = actDf$mrnm
                   newRow$chrSymbol2 = element$chr
                   newRow$mrnm2 = element$mrnm
                   newRow$dir1 = element$dir  # dir1 zawsze niech będzie revers
                   newRow$dir2 = element$dir
                   newRow$chr1Pos = actDf$pos
                   newRow$chr2Pos = element$pos
                   
                   #ctrl + shift + m
                   dfRame %>% mutate_cond((ID == actDf$ID), visited = TRUE) -> dfRame
                   dfRame %>% mutate_cond((ID == element$ID), visited = TRUE) -> dfRame
                   
                   dfRameNewWithGroups =   dplyr::bind_rows(dfRameNewWithGroups,newRow)
                   assign("dfRameNewWithGroups",dfRameNewWithGroups,thisEnv)
                   assign("dfRame",dfRame,thisEnv)
                 }
                 
               }
               
               #tutaj zwroc wynik
               
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
               
               
               
               
             },
             
             getDataFrameWithGroups = function(){
               assign("dfRameNewWithGroups",dfR,thisEnv)
               return(get("dfRameNewWithGroups",thisEnv))
               
             }
             
  )

  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  ## Set the name for the class
  class(me) <- append(class(me),"GroupFilesManager")
  return(me)
}