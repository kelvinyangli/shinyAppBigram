if(!require(shiny))
  install.packages("shiny")
if(!require(DT))
  install.packages("DT")
if(!require(readxl))
  install.packages("readxl")
if(!require(dplyr))
  install.packages("dplyr")
if(!require(tidytext))
  install.packages("tidytext")
if(!require(ggplot2))
  install.packages("ggplot2")
if(!require(tidyr))
  install.packages("tidyr")


library(shiny)
library(DT) # data table format for shiny
library(readxl) # read excel files
library(dplyr) # data wrangling, including filter out missing value
library(tidytext)
library(ggplot2)
library(tidyr)
