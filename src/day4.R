library(tidyverse)
source("src/sharedFunx.R")

day4 = function() {
  input = readInputFile(4)
  
  q1 = bind_cols(
    input, 
    tibble(X1_min = str_split_fixed(input$X1, "-", 2)[,1], 
           X1_max = str_split_fixed(input$X1, "-", 2)[,2],
           X2_min = str_split_fixed(input$X2, "-", 2)[,1], 
           X2_max = str_split_fixed(input$X2, "-", 2)[,2])) %>%
    mutate(self_contained = pmap(list(X1_min, X1_max, X2_min, X2_max), is_subset)) %>%
    mutate(overlap = pmap(list(X1_min, X1_max, X2_min, X2_max), has_intersect)) %>%
    unnest(cols = c(self_contained, overlap))
  
  a1 = sum(q1$self_contained)
  a2 = sum(q1$overlap)
  
  return(returnAnswerRow(4, a1, a2))
}


