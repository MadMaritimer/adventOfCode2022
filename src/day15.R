library(tidyverse)
source("src/sharedFunx.R")

# ROW_OF_INTEREST = -2
# MIN_BOUND = 0
# MAX_BOUND = 20
# input = read_lines("inputs/ex15.txt")
input = read_lines("inputs/input15.txt")
ROW_OF_INTEREST = 2000000
MIN_BOUND = 0
MAX_BOUND = 4000000

calculateManhattanDistance = function(point1, point2) {
  xPos1 = as.numeric(str_split_fixed(point1, ",", 2)[,1])
  yPos1 = as.numeric(str_split_fixed(point1, ",", 2)[,2])
  
  xPos2 = as.numeric(str_split_fixed(point2, ",", 2)[,1])
  yPos2 = as.numeric(str_split_fixed(point2, ",", 2)[,2])
  
  return(abs(yPos2 - yPos1) + abs(xPos2 - xPos1))
}

shortCut_V2 = function(prevPoints, point1, point2){
  xPos = as.numeric(str_split_fixed(point1, ",", 2)[,1])
  yPos = as.numeric(str_split_fixed(point1, ",", 2)[,2])
  
  distance = calculateManhattanDistance(point1, point2)
  if(!between(ROW_OF_INTEREST, yPos-distance, yPos+distance)){
    return(prevPoints)
  }
  yDistance = abs(yPos - ROW_OF_INTEREST)
  xSpreadWidth = distance - yDistance
  xSpread = paste((xPos - xSpreadWidth),(xPos + xSpreadWidth), sep = ",")
  return(c(prevPoints, xSpread))
}

part2 = function(prevPoints, point1, point2){
  print(paste("Starting sensor at ", point1))
  xPos = as.numeric(str_split_fixed(point1, ",", 2)[,1])
  yPos = as.numeric(str_split_fixed(point1, ",", 2)[,2])
  
  distance = calculateManhattanDistance(point1, point2)
  coveredY = (yPos - distance):(yPos + distance)
  targetY = max(MIN_BOUND, yPos-distance) : min(MAX_BOUND, yPos+distance)
  validIndices = which(coveredY %in% targetY)
  xSpreadWidth = distance -abs(-distance:distance)[validIndices]
  xMin = sapply(xPos - xSpreadWidth, function(x) max(x, MIN_BOUND))
  xMax = sapply(xPos + xSpreadWidth, function(x) min(x, MAX_BOUND))
  return(bind_rows(prevPoints, tibble(y = targetY, rangeStart = xMin, rangeEnd = xMax)))
}

mergeScanRanges = function(prevScanCoverage, nextScanRange) {
  xMin = as.numeric(str_split_fixed(nextScanRange, ",", 2)[,1])
  xMax = as.numeric(str_split_fixed(nextScanRange, ",", 2)[,2])
  c(prevScanCoverage, xMin:xMax)
}

day15 = function(showYourWork = FALSE) {
  if(!showYourWork){
    return(returnAnswerRow(15, 4737443, 11482462818989))
  }
  sensors = str_split_fixed(input, ":", 2)[,1]
  sensors = str_extract_all(str_split(str_split_fixed(input, ":", 2)[,1], ","), "-?\\d+")
  sensorsNumeric = lapply(sensors, as.numeric)
  
  beacons = str_extract_all(str_split(str_split_fixed(input, ":", 2)[,2], ","), "-?\\d+")
  beaconsNumeric = lapply(beacons, as.numeric)
  
  
  sensorPairs = lapply(sensors, paste, collapse = ",")
  beaconPairs = lapply(beacons, paste, collapse = ",")
  
  beaconTibble = tibble(x = str_split_fixed(beaconPairs, ",", 2)[,1], y = str_split_fixed(beaconPairs, ",", 2)[,2]) %>%
    mutate(across(1:2, as.numeric))
  
  alreadyOccupied = unique(beaconTibble %>% filter(y == ROW_OF_INTEREST) %>% pull(x))
  shortCutV2Test = reduce2(sensorPairs, beaconPairs, shortCut_V2, .init = c())
  scannedArea = reduce(shortCutV2Test, mergeScanRanges,  .init = c())
  a1 = length(setdiff(unique(scannedArea), alreadyOccupied))
  
  
  part2Ranges = reduce2(sensorPairs, beaconPairs, part2, .init = tibble(y = c(), rangeStart = c(), rangeEnd = c())) %>%
    arrange(y, rangeStart) %>%
    group_by(y) %>%
    mutate(g = cumsum(cummax(lag(rangeEnd, default = first(rangeEnd))) < rangeStart)) %>%
    group_by(y, g) %>%
    summarise(start = first(rangeStart), stop = max(rangeEnd)) %>%
    filter(stop != MAX_BOUND)
  
  a2 = part2Ranges$y[1] + MAX_BOUND * (part2Ranges$stop + 1)
  returnAnswerRow(15, a1, a2)
}

