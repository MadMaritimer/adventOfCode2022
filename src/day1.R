library(tidyverse)
source("src/sharedFunx.R")



day1 = function() {
  input = readInputFile(1)
  q1 = input %>%
    mutate(separator = if_else(is.na(X1), TRUE, FALSE)) %>%
    mutate(elf_id = cumsum(separator)) %>%
    group_by(elf_id) %>%
    summarise(
      totalCalories = sum(X1, na.rm = TRUE)
    )
  
  a1 = max(q1$totalCalories)
  
  q2 = q1 %>% arrange(-totalCalories) %>%
    slice_head(n = 3)
  
  a2 = sum(q2$totalCalories)
  
  return(returnAnswerRow(1, a1, a2))
}
