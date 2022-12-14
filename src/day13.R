library(tidyverse)
library(qdap)
source("src/sharedFunx.R")

listifyTheStrings = function(stringToParse) {
  parsedString = str_replace_all(str_replace_all(stringToParse, "\\[", "list\\("), "\\]", "\\)")
  eval(parse(text = parsedString))
}

compareLists_V2 = function(left, right) {
  status = c(TRUE, FALSE)
  if(length(left) > 0 & length(right) == 0){
    return(c(FALSE, TRUE))
  }
  if(length(left) == 0 & length(right) > 0) {
    return(c(TRUE, TRUE))
  }
  if(length(left) == 0 & length(right) == 0){
    return(status)
  }
  
  for(i in 1:min(length(left), length(right))) {
    if(is.numeric(left[[i]]) & is.numeric(right[[i]])) {
      if(left[[i]] < right[[i]]) {
        return(c(TRUE, TRUE))
      }
      if(left[[i]] > right[[i]]) {
        return(c(FALSE, TRUE))
      }
    } else {
      status = compareLists_V2(left[[i]], right[[i]])
    }
    if(status[2]) {
      return(status)
    }
  }
  if(length(left) > length(right)) {
    return(c(FALSE, TRUE))
  } else if(length(left) < length(right)) {
    return(c(TRUE, TRUE))
  } else
    return(status)
}

orderLists = function(left, right) {
  if(is.null(right)){
    return(0)
  }
  result = compareLists_V2(left, right)
  if(result[1])
    return(-1)
  else
    return(1)
}

'[.packet' <- function(x, i) {
  class(x) <- "list"
  x <- x[i]
  class(x) <- "packet"
  x
}
'>.packet' <- function(a,b) {
  ifelse(orderLists(a, b) == 1, TRUE, FALSE)
}
## if we can't find a difference, then there is no difference
'==.packet' <- function(a, b) 
  ifelse(b > a || a > b, FALSE, TRUE)
## we don't need that, but for the sake of completeness...
'<.packet' <- function(a, b) b > a
  
day13 = function(){
  input = read_lines("inputs/input13.txt")
  #input = read_lines("inputs/ex13.txt")
  
  q1 = as_tibble(input) %>%
    rename(X1 = value) %>%
    mutate(pairId = cumsum(X1 == "") + 1) %>%
    filter(X1 != "") %>%
    group_by(pairId) %>%
    mutate(packetNum = row_number()) %>%
    ungroup() %>%
    pivot_wider(id_cols = pairId, names_from = packetNum, names_prefix = "packet_", values_from = X1)
  
  packet1s = lapply(q1$packet_1, listifyTheStrings)
  packet2s = lapply(q1$packet_2, listifyTheStrings)
  
  q1a = tibble(packet1s, packet2s) %>%
    mutate(sorted = lapply(map2(packet1s, packet2s, compareLists_V2), function(x) x[1]))%>%
    mutate(rowNum = row_number())%>%
    filter(sorted == TRUE)
  
  a1 = sum(q1a$rowNum)
  
  q2 = as_tibble(input) %>%
    rename(X1 = value) %>%
    filter(X1 != "") %>%
    bind_rows(tibble_row(X1 = "[[2]]"), tibble_row(X1 = "[[6]]")) %>%
    mutate(packetId = row_number())
  
  decoderKeyIds = tail(q2$packetId, 2)
  
  q2Packets = lapply(q2$X1, listifyTheStrings)
  
  q2a = tibble(packets = q2Packets) %>%
    mutate(packetId = row_number())
  
  v = list(q2a$packets)[[1]]
  class(v) <- "packet"
  
  
  sortedPackets = sort(v)
  
  class(sortedPackets) = "list"
  
  q2_sorted = tibble(packets = sortedPackets) %>%
    mutate(sortOrder = row_number())
  
  q2c = q2a %>%
    inner_join(q2_sorted) %>%
    filter(packetId %in% !!decoderKeyIds)
  
  a2 = prod(q2c$sortOrder)
  
  returnAnswerRow(13, a1, a2)
}



