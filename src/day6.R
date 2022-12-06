library(tidyverse)
library(zoo)
source("src/sharedFunx.R")

day6 = function() {
  
  input = read_file("inputs/input6.txt")
  explodedInput = unlist(str_split(input, ""))
  q1Input = tibble(symbol = explodedInput)
  
  q1 = q1Input %>%
    mutate(rowId = row_number()) %>%
    mutate(marker = paste(lag(symbol, 3, "#"), lag(symbol, 2, "#"), lag(symbol, 1,"#"), symbol, sep = "")) %>%
    mutate(message = rollapply(data = symbol, width = 14, align = "right", FUN = paste, fill = NA, collapse = "")) %>%
    mutate(startPacketDetected = lapply(lapply(str_split(marker, ""), duplicated),sum) == 0 & rowId >= 4) %>%
    mutate(startMessageDetected = lapply(lapply(str_split(message, ""), duplicated),sum) == 0 & rowId >= 14)
  
  a1 = q1 %>%
    filter(startPacketDetected == TRUE) %>%
    slice_head(n = 1) %>%
    pull(rowId)
  
  a2 = q1 %>%
    filter(startMessageDetected == TRUE) %>%
    slice_head(n = 1) %>%
    pull(rowId)
  
  returnAnswerRow(6,a1,a2)
}



