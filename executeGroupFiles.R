executeGroupFiles = function(){
  
  groupManager <- GroupFilesManager()
  groupManager$getDataFrame()
  groupManager$setNewColumn()
  groupManager$findGroupsInChromosonmes(5000)
  groupManager$getDataFrameWithGroups()
}

browser()
debug(executeGroupFiles())

executeGroupFiles()
