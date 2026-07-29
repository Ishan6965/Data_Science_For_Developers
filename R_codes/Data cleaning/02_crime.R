setwd("D:/Data Science/240013_DataScienceForDev_cw/raw_data/Crime dataset")

crime_files <- list.files(
  recursive = TRUE,
  pattern = "\\.csv$",
  full.names = TRUE
)

crimes <- lapply(crime_files, read.csv, header = TRUE)

for(i in seq_along(crimes)){
  
  colnames(crimes[[i]]) <- c(
    "Crime_ID",
    "Month",
    "Reported_By",
    "Falls_Within",
    "Longitude",
    "Latitude",
    "Location",
    "LSOA_Code",
    "LSOA_Name",
    "Crime_Type",
    "Last_Outcome_Category",
    "Context"
  )
  
  crimes[[i]]$Month <- as.Date(paste0(crimes[[i]]$Month, "-01"))
  
  crimes[[i]]$Reported_By <- factor(crimes[[i]]$Reported_By)
  crimes[[i]]$Falls_Within <- factor(crimes[[i]]$Falls_Within)
  crimes[[i]]$LSOA_Code <- factor(crimes[[i]]$LSOA_Code)
  crimes[[i]]$LSOA_Name <- factor(crimes[[i]]$LSOA_Name)
  crimes[[i]]$Crime_Type <- factor(crimes[[i]]$Crime_Type)
  crimes[[i]]$Last_Outcome_Category <- factor(crimes[[i]]$Last_Outcome_Category)
  
  crimes[[i]]$Context <- NULL
  
  crimes[[i]] <- crimes[[i]][!duplicated(crimes[[i]]), ]
  
  char_cols <- sapply(crimes[[i]], is.character)
  crimes[[i]][char_cols] <- lapply(crimes[[i]][char_cols], function(x){
    x[x == ""] <- "Unknown"
    x
  })
  
  num_cols <- sapply(crimes[[i]], is.numeric)
  crimes[[i]][num_cols] <- lapply(crimes[[i]][num_cols], function(x){
    x[is.na(x)] <- 0
    x
  })
  
  fac_cols <- sapply(crimes[[i]], is.factor)
  crimes[[i]][fac_cols] <- lapply(crimes[[i]][fac_cols], function(x){
    levels(x) <- c(levels(x), "Unknown")
    x[is.na(x)] <- "Unknown"
    x
  })
  
  write.csv(
    crimes[[i]],
    paste0(
      "D:/Data Science/240013_DataScienceForDev_cw/clean_data/crime_clean_",
      i,
      ".csv"
    ),
    row.names = FALSE
  )
  
}


write.csv(
  crime,
  file = "crime_final.csv",
  row.names = FALSE
)