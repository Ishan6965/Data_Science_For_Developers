setwd("D:/Data Science/240013_DataScienceForDev_cw/raw_data/POPN dataset")
setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")

population <- read.csv("population_2024.csv")

names(population) 
dim(population)

names(population)[1:5] <- c(
  "LAD_Code",
  "LAD_Name",
  "LSOA_Code",
  "LSOA_Name",
  "Population"
)

write.csv(
  population,
  "population_2024.csv",
  row.names = FALSE
)

