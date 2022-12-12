library(tidyverse)
source("src/sharedFunx.R")


day11 = function(showYourWork = FALSE) {
  #my shoddy implentation takes WAY too long to run every time, so here's some
  #hardcoded answers for daily answer generation
  if(!showYourWork) {
   return(returnAnswerRow(11, 101436, 19754471646)) 
  }
  
  input = read_delim("inputs/input11.txt", delim = "\n\n", col_names = F)
  
  monkeyInfo = input %>%
    mutate(monkeyId = cumsum(str_detect(X1, "Monkey"))) %>%
    group_by(monkeyId) %>%
    mutate(monkeyRow = row_number())
  
  
  monkeyStartingItems = monkeyInfo %>% filter(monkeyRow == 2) %>%
    mutate(items = str_split_fixed(X1, ": ", 2)[,2]) %>%
    select(monkeyId, items)
  
  monkeyOperation = monkeyInfo %>% filter(monkeyRow == 3) %>%
    mutate(rightSide = str_split_fixed(X1, "= ", 2)[,2]) %>% 
    mutate(operation = str_split_fixed(rightSide, " ", 3)[,2]) %>%
    mutate(opFactor = str_split_fixed(rightSide, " ", 3)[,3]) %>%
    select(monkeyId, operation, opFactor)
  
  monkeyTestDivisor = monkeyInfo %>% filter(monkeyRow == 4) %>%
    mutate(testDivisor = as.numeric(str_split_fixed(X1, " Test: divisible by", 2)[,2])) %>%
    select(monkeyId, testDivisor)
  
  trueResultMonkey = monkeyInfo %>% filter(monkeyRow == 5) %>%
    mutate(trueMonkey = trimws(str_split_fixed(X1, " If true: throw to monkey", 2)[,2]))  %>%
    select(monkeyId, trueMonkey)
  
  falseResultMonkey = monkeyInfo %>% filter(monkeyRow == 6) %>%
    mutate(falseMonkey = trimws(str_split_fixed(X1, " If false: throw to monkey", 2)[,2])) %>%
    select(monkeyId, falseMonkey)
  
  
  monkeyTable = monkeyOperation %>%
    inner_join(monkeyTestDivisor) %>%
    inner_join(trueResultMonkey) %>%
    inner_join(falseResultMonkey) %>%
    inner_join(monkeyStartingItems) %>%
    mutate(monkeyId = monkeyId -1)
  
  monkeyLists = lapply(str_split(monkeyTable$items, ","), as.numeric)
  names(monkeyLists) = monkeyTable$monkeyId
  monkeyItemCounts = rep(0, nrow(monkeyTable))
  
  monkeyTest = function(monkey, itemValue) {
    myMonkey = monkeyTable %>% filter(monkeyId == monkey)
    worryFactor = if_else(myMonkey$opFactor[1] == "old", itemValue, as.numeric(myMonkey$opFactor[1]))
    totalWorry = if_else(myMonkey$operation == "+", itemValue + worryFactor, itemValue * worryFactor)
    calmedWorry = floor(totalWorry / 3)
    if(calmedWorry %% myMonkey$testDivisor == 0){
      monkeyLists[[as.character(myMonkey$trueMonkey)]] <<- c(monkeyLists[[as.character(myMonkey$trueMonkey)]], calmedWorry)
    } else{
      monkeyLists[[as.character(myMonkey$falseMonkey)]] <<- c(monkeyLists[[as.character(myMonkey$falseMonkey)]], calmedWorry)
    }
    monkeyItemCounts[monkey+1] <<- monkeyItemCounts[monkey+1] + 1
  }
  
  LCM = prod(monkeyTable$testDivisor)
  monkeyTest_V2 = function(monkey, itemValue, LCM) {
    myMonkey = monkeyTable %>% filter(monkeyId == monkey)
    worryFactor = if_else(myMonkey$opFactor[1] == "old", itemValue, as.numeric(myMonkey$opFactor[1]))
    totalWorry = if_else(myMonkey$operation == "+", itemValue + worryFactor, itemValue * worryFactor)
    calmedWorry = totalWorry %% LCM
    if(calmedWorry %% myMonkey$testDivisor == 0){
      q2_monkeyLists[[as.character(myMonkey$trueMonkey)]] <<- c(q2_monkeyLists[[as.character(myMonkey$trueMonkey)]], calmedWorry)
    } else{
      q2_monkeyLists[[as.character(myMonkey$falseMonkey)]] <<- c(q2_monkeyLists[[as.character(myMonkey$falseMonkey)]], calmedWorry)
    }
    q2_monkeyItemCounts[monkey+1] <<- q2_monkeyItemCounts[monkey+1] + 1
  }
  
  for(round in 1:20) {
    for (monkey in 0:nrow(monkeyTable) - 1) {
      for (item in monkeyLists[[as.character(monkey)]]) {
        monkeyTest(monkey, item)
      }
      monkeyLists[[as.character(monkey)]] <- c()
    }
  }
  
  q1_top2 = head(sort(monkeyItemCounts, decreasing = T), 2)
  a1 = prod(q1_top2)
  
  
  q2_monkeyLists = lapply(str_split(monkeyTable$items, ","), as.numeric)
  names(q2_monkeyLists) = monkeyTable$monkeyId
  q2_monkeyItemCounts = rep(0, nrow(monkeyTable))
  for(round in 1:10000) {
    for (monkey in 0:nrow(monkeyTable) - 1) {
      for (item in q2_monkeyLists[[as.character(monkey)]]) {
        monkeyTest_V2(monkey, item, LCM)
      }
      q2_monkeyLists[[as.character(monkey)]] <- c()
    }
    print(paste(round, Sys.time(), sep = "---"))
  }
  
  
  q2_top2 = head(sort(q2_monkeyItemCounts, decreasing = T), 2)
  a2 = prod(q2_top2)
  
  returnAnswerRow(11, a1, a2)
}













