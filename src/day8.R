library(tidyverse)
library(zoo)
source("src/sharedFunx.R")



visibleFromTop = function(prevRows) {
  rowNumber = length(prevRows)
  if(rowNumber == 1) {
   return(TRUE)
 }
  maxPos = which.max(prevRows)
  isVisible = maxPos == rowNumber
  return(isVisible)
}

viewDistance = function(prevRows) {
  if(length(prevRows) == 1){
    return(0)
  }
  sightLine = rev(prevRows)
  sightLineMask = sightLine[-1] >= sightLine[1]
  
  return(match(TRUE, sightLineMask, nomatch = length(sightLineMask)))
}

day8 = function() {
  input = readInputFile(8)
  
  inputMatrix = str_split_fixed(input$X1, "", n= 99)
  
  q1 = as_tibble(inputMatrix) %>%
    mutate(rowNumber = row_number()) %>%
    relocate(rowNumber)
  
  visibility_Top = q1 %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = visibleFromTop, align = "right"))) %>%
    select(-rowNumber)
  
  visibility_Bottom = q1 %>%
    arrange(-rowNumber) %>%
    mutate(rowNumber = row_number()) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = visibleFromTop, align = "right"))) %>%
    arrange(-rowNumber) %>%
    select(-rowNumber)
  
  visibility_Left = as_tibble(t(q1 %>% select(-rowNumber))) %>%
    mutate(rowNumber = row_number()) %>%
    relocate(rowNumber) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = visibleFromTop, align = "right"))) %>%
    select(-rowNumber) %>%
    t() %>%
    as_tibble()
  
  visibility_Right = as_tibble(t(q1 %>% select(-rowNumber))) %>%
    mutate(rowNumber = row_number()) %>%
    relocate(rowNumber) %>%
    arrange(-rowNumber) %>%
    mutate(rowNumber = row_number()) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = visibleFromTop, align = "right"))) %>%
    arrange(-rowNumber) %>%
    select(-rowNumber) %>%
    t() %>%
    as_tibble()
  
  visibility_Full = visibility_Top | visibility_Bottom | visibility_Left | visibility_Right
  
  a1 = sum(visibility_Full)
  
  
  sightline_Top = q1 %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = viewDistance, align = "right"))) %>%
    select(-rowNumber)
  
  sightline_Bottom = q1 %>%
    arrange(-rowNumber) %>%
    mutate(rowNumber = row_number()) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = viewDistance, align = "right"))) %>%
    arrange(-rowNumber) %>%
    select(-rowNumber)
  
  sightline_Left = as_tibble(t(q1 %>% select(-rowNumber))) %>%
    mutate(rowNumber = row_number()) %>%
    relocate(rowNumber) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = viewDistance, align = "right"))) %>%
    select(-rowNumber) %>%
    t() %>%
    as_tibble()
  
  sightline_Right = as_tibble(t(q1 %>% select(-rowNumber))) %>%
    mutate(rowNumber = row_number()) %>%
    relocate(rowNumber) %>%
    arrange(-rowNumber) %>%
    mutate(rowNumber = row_number()) %>%
    mutate(across(2:100, ~rollapply(.x, width = rowNumber, FUN = viewDistance, align = "right"))) %>%
    arrange(-rowNumber) %>%
    select(-rowNumber) %>%
    t() %>%
    as_tibble()
  
  
  scenicScore = sightline_Top * sightline_Bottom * sightline_Left * sightline_Right
  a2 = max(scenicScore)
  
  return(returnAnswerRow(8, a1, a2))
}




