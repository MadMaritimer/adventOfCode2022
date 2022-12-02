library(tidyverse)
source("src/sharedFunx.R")
source("src/day1.R")

answerTable = bind_rows(
  day1(),
  day2()
)
