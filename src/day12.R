library(tidyverse)
library(igraph)
source("src/sharedFunx.R")

day12 = function() {
  input = read_lines("inputs/input12.txt")
  #input = read_lines("inputs/ex12.txt")
  explodedInput = matrix(unlist(str_split(input, "")), nrow = length(input), ncol = nchar(input[1]), byrow = TRUE) 
  
  startPos = which(explodedInput == "S")
  endPos = which(explodedInput == "E")
  
  q1 = as_tibble(explodedInput) %>%
    mutate(across(everything(), ~if_else(.x == "S", "a", .x))) %>%
    mutate(across(everything(), ~match(.x, letters, 26)))
  
  canMoveUp = q1 %>%
    mutate(across(everything(), ~if_else(is.na(lag(.x, 1)) | .x < lag(.x, 1) - 1 , FALSE, TRUE))) %>%
    as.matrix()
  canMoveUp[which(canMoveUp %in% TRUE)] = which(canMoveUp %in% TRUE) - 1
  
  canMoveDown = q1 %>%
    mutate(across(everything(), ~if_else(is.na(lead(.x, 1)) | .x < lead(.x, 1) - 1 , FALSE, TRUE))) %>%
    as.matrix()
  canMoveDown[which(canMoveDown %in% TRUE)] = which(canMoveDown %in% TRUE) + 1
  
  canMoveLeft = as_tibble(t(q1)) %>% 
    mutate(across(everything(), ~if_else(is.na(lag(.x, 1)) | .x < lag(.x, 1) - 1 , FALSE, TRUE))) %>%
    t()
  canMoveLeft[which(canMoveLeft %in% TRUE)] = which(canMoveLeft %in% TRUE) - nrow(q1)
  
  canMoveRight = as_tibble(t(q1)) %>% 
    mutate(across(everything(), ~if_else(is.na(lead(.x, 1)) | .x < lead(.x, 1) - 1 , FALSE, TRUE))) %>%
    t()
  canMoveRight[which(canMoveRight %in% TRUE)] = which(canMoveRight %in% TRUE) + nrow(q1)
  
  
  filteredMoves = lapply(pmap(list(canMoveUp, canMoveDown, canMoveLeft, canMoveRight), c),  function(x) x[x!=0])
  names(filteredMoves) = as.character(1:length(filteredMoves))
  edge_list = as.matrix(as_tibble(stack(filteredMoves)) %>% relocate(2, 1) %>% mutate(ind = as.numeric(ind)))
  g <- graph_from_edgelist(edge_list, directed = T)
  
  
  myPath = shortest_paths(g, from = startPos, to = endPos)
  a1 = length(myPath[["vpath"]][[1]]) - 1
  
  allPossibleStarts = which(explodedInput %in% c("a", "S"))
  results = lapply(allPossibleStarts, shortest_paths, graph = g, to = endPos)
  lengths = unlist(lapply(results, function(x) length(x[["vpath"]][[1]]) - 1))
  a2 = min(lengths[lengths>0])
  
  return(returnAnswerRow(12, a1, a2))
}



