library(tidyverse)
library(zoo)
source("src/sharedFunx.R")

TOTAL_SPACE = 70000000
SPACE_FOR_UPDATE = 30000000

tracePath = function(prevRows) {
  backupPositions = which(prevRows %in% "..")
  backupCounts = length(backupPositions)
  while (backupCounts != 0) {
    prevRows = prevRows[-c(backupPositions[1], backupPositions[1] -1)]
    backupPositions = which(prevRows %in% "..")
    backupCounts = length(backupPositions)
  }
  paste(prevRows, collapse = "#")
}

sumChildren = function(currPath, sizeTable) {
  children = sizeTable %>%
    filter(str_detect(parentPath, currPath))
  return(sum(children$localSize))
}


day7 = function() {
  input = readInputFile(7)
  
  q1 = input %>%
    mutate(rowId = row_number()) %>%
    mutate(objType = if_else(str_detect(X1, "\\$"), "COMMAND",
                             if_else(str_detect(X1, "\\d"), "FILE", "DIR")))
  
  q1_cd_commands = q1 %>% filter(objType == "COMMAND" & str_detect(X1, "\\$ cd")) %>%
    mutate(destination = str_split_fixed(X1, " ", 3)[,3]) %>%
    mutate(commandCount = row_number()) %>%
    mutate(parentPath = rollapply(destination, width = commandCount, FUN = tracePath, align = "right"))
  
  q1_paths = q1 %>% 
    left_join(q1_cd_commands) %>%
    fill(parentPath)
  
  q1_filePaths = q1_paths %>%
    filter(objType == "FILE" | objType == "DIR") %>%
    mutate(size = as.numeric(str_split_fixed(X1, " ", 2)[,1])) %>%
    mutate(size = if_else(is.na(size), 0, size))
  
  q1_dirSizes = q1_filePaths %>%
    group_by(parentPath) %>%
    summarise(
      localSize = sum(size)
    ) 
  
  q1_dirSizes$fullSize = unlist(lapply(q1_dirSizes$parentPath, sumChildren, sizeTable = q1_dirSizes))
  
  q1_filtered = q1_dirSizes %>% filter(fullSize <= 100000)
  
  a1 = sum(q1_filtered$fullSize)
  
  
  spaceInUse = q1_dirSizes$fullSize[1]
  freeSpace = TOTAL_SPACE - spaceInUse
  spaceToFree = SPACE_FOR_UPDATE - freeSpace
  
  q2 = q1_dirSizes %>% filter(fullSize > spaceToFree) %>%
    arrange(fullSize)
  
  a2 = q2$fullSize[1]
  
  return(returnAnswerRow(7, a1, a2))
}



