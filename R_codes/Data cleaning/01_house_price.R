setwd("D:/Data Science/240013_DataScienceForDev_cw/raw_data/Price dataset")

getwd()
list.files()
list.files(recursive = TRUE)

house2021 <- read.csv("pp-2021.csv", header = FALSE)
house2022 <- read.csv("pp-2022.csv", header = FALSE)
house2023 <- read.csv("pp-2023.csv", header = FALSE)
house2024 <- read.csv("pp-2024.csv", header = FALSE)
house2025 <- read.csv("pp-2025.csv", header = FALSE)

houses <- list(
  house2021,
  house2022,
  house2023,
  house2024,
  house2025
)


houses <- list(house2021, house2022, house2023, house2024, house2025)
years <- 2021:2025

col_names <- c(
  "Transaction_ID", "Price", "Date", "Postcode",
  "Property_Type", "New_Build", "Tenure",
  "Primary_Address", "Secondary_Address",
  "Street", "Locality", "Town_City",
  "District", "County",
  "PPD_Category", "Record_Status"
)

for(i in 1:length(houses)){
  
  colnames(houses[[i]]) <- col_names
  
  houses[[i]]$Date <- as.Date(substr(houses[[i]]$Date, 1, 10))
  
  houses[[i]]$Property_Type <- factor(houses[[i]]$Property_Type)
  houses[[i]]$New_Build <- factor(houses[[i]]$New_Build)
  houses[[i]]$Tenure <- factor(houses[[i]]$Tenure)
  houses[[i]]$PPD_Category <- factor(houses[[i]]$PPD_Category)
  houses[[i]]$Record_Status <- factor(houses[[i]]$Record_Status)
  
  write.csv(
    houses[[i]],
    paste0(
      "D:/Data Science/240013_DataScienceForDev_cw/clean_data",
      years[i],
      "_clean.csv"
    ),
    row.names = FALSE
  )
}



write.csv(
  house,
  "D:/Data Science/240013_DataScienceForDev_cw/clean_data/house_final.csv",
  row.names = FALSE
)