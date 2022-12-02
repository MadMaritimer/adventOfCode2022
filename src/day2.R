library(tidyverse)
source("src/sharedFunx.R")

day2 = function() {
  input = readInputFile(2)
  
  winningDiffs = c(-2, 1)
  
  q1 = input %>%
    separate(X1, into = c("Elf", "Me")) %>%
    mutate(me_numeric = if_else(Me == "X", 1, if_else(Me == "Y", 2, 3))) %>%
    mutate(elf_numeric = if_else(Elf == "A", 1, if_else(Elf == "B", 2, 3))) %>%
    mutate(diffCol = me_numeric - elf_numeric) %>%
    mutate(score = me_numeric + if_else(diffCol == 0, 3, if_else(diffCol %in% winningDiffs, 6, 0)))
  
  a1 = sum(q1$score)
  
  q2 = input %>%
    separate(X1, into = c("Elf", "Result")) %>%
    mutate(elf_mod3 = if_else(Elf == "A", 0, if_else(Elf == "B", 1, 2))) %>%
    mutate(me_mod3 = if_else(Result == "Y", elf_mod3, if_else(Result == "X", (elf_mod3 - 1) %% 3, (elf_mod3 + 1) %% 3))) %>%
    mutate(score = me_mod3 + 1 + if_else(Result == "X", 0, if_else(Result == "Y", 3, 6)))
  
  a2 = sum(q2$score)
  return(returnAnswerRow(2, a1, a2))
}

