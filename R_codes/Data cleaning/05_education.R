setwd("D:/Data Science/240013_DataScienceForDev_cw/clean_data/education_dataset")

#-----------------------------
# School datasets
#-----------------------------

school_2021 <- read.csv(
  "2021-2022/school_2021_2022_clean.csv",
  stringsAsFactors = FALSE
)

school_2022 <- read.csv(
  "2022-2023/school_2022_2023_clean.csv",
  stringsAsFactors = FALSE
)

school_2023 <- read.csv(
  "2023-2024/school_2023_2024_clean.csv",
  stringsAsFactors = FALSE
)

school_2024 <- read.csv(
  "2024-2025/school_2024_2025_clean.csv",
  stringsAsFactors = FALSE
)

# Add missing columns
school_2023$Ofsted_Rating <- NA
school_2023$Last_Ofsted_Inspection <- NA

school_2024$Ofsted_Rating <- NA
school_2024$Last_Ofsted_Inspection <- NA

# Match column order
school_2023 <- school_2023[, names(school_2021)]
school_2024 <- school_2024[, names(school_2021)]

# Combine school datasets
school_final <- rbind(
  school_2021,
  school_2022,
  school_2023,
  school_2024
)

#-----------------------------
# KS4 datasets
#-----------------------------

ks4_2021 <- read.csv(
  "2021-2022/ks4_2021_2022_clean.csv",
  stringsAsFactors = FALSE
)

ks4_2022 <- read.csv(
  "2022-2023/ks4_2022_2023_clean.csv",
  stringsAsFactors = FALSE
)

ks4_2023 <- read.csv(
  "2023-2024/ks4_2023_2024_clean.csv",
  stringsAsFactors = FALSE
)

ks4_2024 <- read.csv(
  "2024-2025/ks4_2024_2025_clean.csv",
  stringsAsFactors = FALSE
)

# Keep only columns common to all years
common_cols <- Reduce(
  intersect,
  list(
    names(ks4_2021),
    names(ks4_2022),
    names(ks4_2023),
    names(ks4_2024)
  )
)

ks4_2021 <- ks4_2021[, common_cols]
ks4_2022 <- ks4_2022[, common_cols]
ks4_2023 <- ks4_2023[, common_cols]
ks4_2024 <- ks4_2024[, common_cols]

# Combine KS4 datasets
ks4_final <- rbind(
  ks4_2021,
  ks4_2022,
  ks4_2023,
  ks4_2024
)

#-----------------------------
# Save combined datasets
#-----------------------------

write.csv(
  school_final,
  "D:/Data Science/240013_DataScienceForDev_cw/merged_data/school_final.csv",
  row.names = FALSE
)

write.csv(
  ks4_final,
  "D:/Data Science/240013_DataScienceForDev_cw/merged_data/ks4_final.csv",
  row.names = FALSE
)


ncol(school_2021)
ncol(school_2022)
ncol(school_2023)
ncol(school_2024)


ncol(ks4_2021)
ncol(ks4_2022)
ncol(ks4_2023)
ncol(ks4_2024)
