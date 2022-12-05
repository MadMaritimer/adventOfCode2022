library(tidyverse)
source("src/sharedFunx.R")

day5 = function() {
  startingPositionsInput = read_lines("inputs/input5.txt", skip = 0, n_max = 8)
  spaced = paste(" ", startingPositionsInput, sep = "")
  replaced = str_replace_all(spaced, "\\s{4}", " [X]")
  aligned = matrix(str_trim(replaced))
  splitCells = str_split(str_trim(replaced), "\\s+", )
  mat = do.call("cbind", splitCells)
  transposedLists = as.list(data.frame(t(mat)))
  startingLists = lapply(transposedLists, removePlaceholders)
  
  workingLists = startingLists
  
  instructionsInput = read_delim(file = "inputs/input5.txt", skip = 10, delim = " ", col_names = FALSE) %>%
    rename(
      code = X1,
      amt = X2,
      from = X3,
      src = X4, 
      to = X5, 
      dest = X6
    )
  
  for (row in 1:nrow(instructionsInput)) {
    if(row == 1)
      workingLists = startingLists
    
    instruction = instructionsInput[row,]
    amt = instruction$amt[1]
    src = instruction$src[1]
    dest = instruction$dest[1]
    
    inTransit = rev(workingLists[[src]][1:amt])
    workingLists[[src]] = tail(workingLists[[src]], -amt)
    workingLists[[dest]] = c(inTransit, workingLists[[dest]])
  }
  
  tops = c(unlist(lapply(workingLists, function(pile) pile[[1]])))
  a1 = str_replace_all(paste(tops, collapse = ""), "\\[|\\]", "")
  
  for (row in 1:nrow(instructionsInput)) {
    if(row == 1)
      workingLists = startingLists
    
    instruction = instructionsInput[row,]
    amt = instruction$amt[1]
    src = instruction$src[1]
    dest = instruction$dest[1]
    
    inTransit = workingLists[[src]][1:amt]
    workingLists[[src]] = tail(workingLists[[src]], -amt)
    workingLists[[dest]] = c(inTransit, workingLists[[dest]])
  }
  
  tops = c(unlist(lapply(workingLists, function(pile) pile[[1]])))
  a2 = str_replace_all(paste(tops, collapse = ""), "\\[|\\]", "")
  return(returnAnswerRow(5, a1, a2))
}

removePlaceholders = function(myVector) {
  return(myVector[myVector != "[X]"])
}