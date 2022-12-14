library(tidyverse)
source("src/sharedFunx.R")


interpolateWallFromCorners = function(prevPoints, nextPoint) {
  startPoint = prevPoints[length(prevPoints)] 
  
  startX = as.numeric(str_split_fixed(startPoint, ",", 2)[,1])
  startY = as.numeric(str_split_fixed(startPoint, ",", 2)[,2])
  
  nextX = as.numeric(str_split_fixed(nextPoint, ",", 2)[,1])
  nextY = as.numeric(str_split_fixed(nextPoint, ",", 2)[,2]) 
  
  pointsToAdd =  tail(paste(startX:nextX, startY:nextY, sep = ","), -1)
  return(c(prevPoints, pointsToAdd))
}

convertLineStringToPoints = function(rockLineString) {
  points = unlist(str_split(rockLineString, " -> "))
  reduce(points, interpolateWallFromCorners)
}

plotWallPoints = function(visMatrix, pointList, xScaling) {
  xVals = as.numeric(str_split_fixed(pointList, ",", 2)[,1]) - xScaling
  yVals = as.numeric(str_split_fixed(pointList, ",", 2)[,2])
  
  linearPos = (xVals - 1) * nrow(visMatrix) + yVals
  visMatrix[linearPos] = "#"
  return(visMatrix)
}

addFloor = function(visMatrix) {
  floorValues = 1:(ncol(visMatrix))
  floorValues = floorValues * nrow(visMatrix)
  visMatrix[floorValues] = "#"
  return(visMatrix)
}

raiseTheRoof = function(visMatrix) {
  moreSky = matrix(rep(".", ncol(visMatrix)), nrow = 1, ncol = ncol(visMatrix))
  return(rbind(moreSky, visMatrix))
}

addSand = function(visMatrix, startPoint) {
  #check if insert point valid
  startX = as.numeric(str_split_fixed(startPoint, ",", 2)[,1])
  startY = as.numeric(str_split_fixed(startPoint, ",", 2)[,2])
  if(visMatrix[startY, startX] != ".") {
    return(list(FALSE, visMatrix))
  }
  #next check if on bottom bound
  if(startY+1 > nrow(visMatrix)){
    return(list(FALSE, visMatrix))
  }
  #next check directly below, if clear move down
  if(visMatrix[startY+1, startX] == ".") {
    return(addSand(visMatrix, paste(startX, startY+1, sep = ",")))
  }
  #next check if on left bound
  if(startX-1 <= 0){
    return(list(FALSE, visMatrix))
  }
  #next check down left, if clear move
  if(visMatrix[startY+1, startX - 1] == ".") {
    return(addSand(visMatrix, paste(startX - 1, startY + 1, sep = ",")))
  }
  #next check if on right bound
  if(startX+1 > ncol(visMatrix)){
    return(list(FALSE, visMatrix))
  }
  #next check down right,if clear move
  if(visMatrix[startY+1, startX + 1] == ".") {
    return(addSand(visMatrix, paste(startX + 1, startY + 1, sep = ",")))
  }
  visMatrix[startY, startX] = "o"
  return(list(TRUE, visMatrix))
}

day14 = function(showYourWork = FALSE){
  if(!showYourWork){
    return(returnAnswerRow(14, 698, 28594))
  }
  #rockLines = read_lines("inputs/ex14.txt")
  rockLines = read_lines("inputs/input14.txt")
  
  pairs = unlist(str_split(rockLines, " -> "))
  xValues = as.numeric(str_split_fixed(pairs, ",", 2)[,1])
  minX = min(xValues)
  maxX = max(xValues)
  
  yValues =  as.numeric(str_split_fixed(pairs, ",", 2)[,2])
  yValues = yValues
  maxY = max(yValues)
  
  
  scanFill = rep(".", (maxX-minX + 1) * maxY)
  
  scanVis = matrix(scanFill, nrow = maxY, ncol =  (maxX-minX) + 1)
  walls = unlist(lapply(rockLines, convertLineStringToPoints))
  
  scanVis1 = plotWallPoints(scanVis, walls, minX-1)
  scanVis1 = raiseTheRoof(scanVis1)
  
  
  moreSand = TRUE
  while(moreSand){
    results = addSand(scanVis1, paste(500-minX+1, 1, sep = ","))
    scanVis1 = results[[2]]
    moreSand = results[[1]]
  }
  
  a1 = length(scanVis1[scanVis1 == "o"])
  
  scanFill2 = rep(".", (1000) * (maxY+2))
  scanVis2 = matrix(scanFill2, nrow = maxY+2, ncol =  1000)
  walls = unlist(lapply(rockLines, convertLineStringToPoints))
  scanVis2 = plotWallPoints(scanVis2, walls, 0)
  scanVis2 = addFloor(scanVis2) %>% raiseTheRoof()
  
  
  moreSand = TRUE
  while(moreSand){
    results = addSand(scanVis2, paste(500, 1, sep = ","))
    scanVis2 = results[[2]]
    moreSand = results[[1]]
    sandPlaced= sandPlaced+1
    print(sandPlaced)
  }
  a2 = length(scanVis2[scanVis2 == "o"])
  returnAnswerRow(14, a1, a2)
}



