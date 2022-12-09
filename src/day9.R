library(tidyverse)
source("src/sharedFunx.R")


input = readInputFile(9)

fillInHeadPath = function(prevPoints, nextPoint) {
  startPoint = prevPoints[length(prevPoints)] 
  
  startX = as.numeric(str_split_fixed(startPoint, ",", 2)[,1])
  startY = as.numeric(str_split_fixed(startPoint, ",", 2)[,2])
  
  nextX = as.numeric(str_split_fixed(nextPoint, ",", 2)[,1])
  nextY = as.numeric(str_split_fixed(nextPoint, ",", 2)[,2])
  
  pointsToAdd =  tail(paste(startX:nextX, startY:nextY, sep = ","), -1)
  return(c(prevPoints, pointsToAdd))
}


calculateTailPos = function(tailCoords, headCoords) {
  tailX = as.numeric(str_split_fixed(tailCoords, ",", 2)[,1])
  tailY = as.numeric(str_split_fixed(tailCoords, ",", 2)[,2])
  
  headX = as.numeric(str_split_fixed(headCoords, ",", 2)[,1])
  headY = as.numeric(str_split_fixed(headCoords, ",", 2)[,2])
  
  diffX = headX - tailX
  diffY = headY - tailY
  
  if(abs(diffX) < 2 & abs(diffY) < 2) {
    return(tailCoords)
  }
  
  if(abs(diffX) >= 2 & abs(diffY) >= 2) {
    uselessStopLine = 4
    return(paste(tailX + min(match(diffX > 0, TRUE, nomatch = -1), 1),
                 tailY + min(match(diffY > 0, TRUE, nomatch = -1), 1), sep = ","))
  }
  
  if(abs(diffX) >= 2){
    return(paste(tailX + min(match(diffX > 0, TRUE, nomatch = -1), 1), headY, sep = ",")) 
  }
  
  if(abs(diffY) >= 2)
    return(paste(headX, tailY + min(match(diffY > 0, TRUE, nomatch = -1), 1), sep = ","))
}

day9 = function() {
  q1 = input %>%
    mutate(direction = str_split_fixed(X1," ", n = 2)[,1]) %>%
    mutate(steps = as.numeric(str_split_fixed(X1," ", n = 2)[,2])) %>%
    mutate(deltaX = if_else(direction == "L", -steps, 
                            if_else(direction == "R", steps, 0))) %>%
    mutate(deltaY = if_else(direction == "D", -steps, 
                            if_else(direction == "U", steps, 0))) %>%
    mutate(xPos = cumsum(deltaX)) %>%
    mutate(yPos = cumsum(deltaY)) %>%
    mutate(headCoords = paste(xPos, yPos, sep = ","))
  
  
  posTracking = tibble(head = reduce(q1$headCoords, .init = "0,0", .f = fillInHeadPath)) %>%
    mutate(tailCoords = tail(accumulate(head, .f = calculateTailPos, .init = "0,0"), -1))
  
  a1 = length(unique(posTracking$tailCoords))
  
  posTracking = posTracking %>%
    mutate(knot1 = tail(accumulate(head, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot2 = tail(accumulate(knot1, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot3 = tail(accumulate(knot2, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot4 = tail(accumulate(knot3, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot5 = tail(accumulate(knot4, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot6 = tail(accumulate(knot5, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot7 = tail(accumulate(knot6, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot8 = tail(accumulate(knot7, .f = calculateTailPos, .init = "0,0"), -1)) %>%
    mutate(knot9 = tail(accumulate(knot8, .f = calculateTailPos, .init = "0,0"), -1))
  
  a2 = length(unique(posTracking$knot9))
  
  return(returnAnswerRow(9, a1, a2))
}
