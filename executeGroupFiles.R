executeGroupFiles = function(){
  
  groupManager <- GroupFilesManager()
  dataFrame = groupManager$getDataFrame()
  groupManager$setNewColumn()
  groupManager$findGroupsInChromosonmes(5000)
}

executeGroupFiles()