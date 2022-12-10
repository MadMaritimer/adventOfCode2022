library(tidyverse)
source("src/sharedFunx.R")

INTERESTING_CYCLES = c(20, 60, 100, 140, 180, 220)

accumulateCycles = function(cycleCount, nextOp) {
  if(nextOp == "addx"){
    return(cycleCount + 2)
  }
  if(nextOp == "noop"){
    return(cycleCount + 1)
  }
}

day10 = function() {
  input = readInputFile(10)
  
  q1 = input %>%
    mutate(opCode = str_split_fixed(X1, " ", 2)[,1]) %>%
    mutate(deltaX = as.numeric(str_split_fixed(X1, " ", 2)[,2])) %>%
    mutate(cycleCount = tail(accumulate(opCode, accumulateCycles, .init = 0), -1)) %>%
    mutate(deltaX = if_else(is.na(deltaX), 0, deltaX)) %>%
    mutate(xVal = tail(accumulate(deltaX, `+`, .init = 1), -1))
  
  oneToOneCycles = tibble(cycleCount = 1:max(q1$cycleCount)) %>%
    left_join(q1) %>%
    fill(xVal, .direction = "down") %>%
    mutate(xVal = replace_na(xVal, 1)) %>%
    mutate(signalStrength = cycleCount * xVal) %>%
    mutate(cursor = rep(0:39, 6)) %>% 
    mutate(pixel = if_else(cursor >= lag(xVal, 1, 1) - 1 & cursor <= lag(xVal, 1, 1) + 1, "#", "."))
  
  
  a1 = sum(oneToOneCycles$signalStrength[INTERESTING_CYCLES])
  
  #No clean way to get this answer out programmatically, just have to look at the printout and provide the answer
  cat(paste(oneToOneCycles$pixel[1:40], sep = ""), "\n")
  cat(paste(oneToOneCycles$pixel[41:80], sep = ""), "\n")
  cat(paste(oneToOneCycles$pixel[81:120], sep = ""), "\n")
  cat(paste(oneToOneCycles$pixel[121:160], sep = ""), "\n")
  cat(paste(oneToOneCycles$pixel[161:200], sep = ""), "\n")
  cat(paste(oneToOneCycles$pixel[201:240], sep = ""), "\n")
  
  a2 = "PGHFGLUG"
  return(returnAnswerRow(10, a1,a2))
}





