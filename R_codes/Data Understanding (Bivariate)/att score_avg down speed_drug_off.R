setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")
library(dplyr)
library(ggplot2)

performance <- read.csv(
  "internet_perf_final.csv",
  stringsAsFactors = FALSE
)


crime_final <- read.csv(
  "crime_final.csv",
  stringsAsFactors = FALSE
)

population_final <- read.csv(
  "population_2024.csv",
  stringsAsFactors = FALSE
)

ks4_final <- read.csv("ks4_final.csv")




#Attantment score vs drug off rate

ks4_final$Attainment8 <- as.numeric(ks4_final$Attainment8)

# Assign schools to districts using postcode districts

ks4_selected <- ks4_final %>%
  mutate(
    Postcode_District = sub(" .*", "", Postcode),
    District = case_when(
      Postcode_District %in% c("NR17", "NR19", "NR20", "IP24") ~ "BRECKLAND",
      Postcode_District %in% c("PE30", "PE31", "PE32", "PE33", "PE34", "PE35") ~ "KING'S LYNN AND WEST NORFOLK",
      Postcode_District %in% c("IP27", "IP28", "IP29", "IP30", "IP31", "IP32", "IP33") ~ "WEST SUFFOLK",
      Postcode_District %in% c("IP11", "IP12", "IP13", "IP15", "IP16", "IP17", "IP18", "IP19") ~ "EAST SUFFOLK",
      Postcode_District %in% c("IP1", "IP2", "IP3", "IP4", "IP5", "IP6", "IP7", "IP8", "IP9", "IP10") ~ "IPSWICH",
      Postcode_District %in% c("NR1", "NR2", "NR3", "NR4", "NR5", "NR6", "NR7", "NR8") ~ "NORWICH",
      Postcode_District %in% c("NR29", "NR30", "NR31") ~ "GREAT YARMOUTH"
    )
  ) %>%
  filter(
    !is.na(District),
    !is.na(Attainment8)
  )

# Average Attainment 8 score by district

attainment <- ks4_selected %>%
  group_by(District) %>%
  summarise(
    Avg_Attainment8 = mean(Attainment8, na.rm = TRUE),
    .groups = "drop"
  )

# Population by district

population <- population_final %>%
  group_by(LAD_Name) %>%
  summarise(
    Population = sum(Population),
    .groups = "drop"
  ) %>%
  mutate(
    LAD_Name = toupper(LAD_Name)
  )

# Drug offence rate by district

drug_rate <- crime_final %>%
  mutate(
    District = toupper(sub(" [0-9].*", "", LSOA_Name))
  ) %>%
  filter(
    Crime_Type == "Drugs",
    Month == "2024-11-01",
    District %in% selected
  ) %>%
  group_by(District) %>%
  summarise(
    Drug_Count = n(),
    .groups = "drop"
  ) %>%
  left_join(
    population,
    by = c("District" = "LAD_Name")
  ) %>%
  mutate(
    Drug_Rate = (Drug_Count / Population) * 100000
  ) %>%
  select(District, Drug_Rate)

# Merge datasets

attainment_drug <- left_join(
  attainment,
  drug_rate,
  by = "District"
)

# Scatter plot with line of best fit

ggplot(attainment_drug,
       aes(x = Avg_Attainment8,
           y = Drug_Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Attainment 8 Score vs Drug Offence Rate",
    x = "Average Attainment 8 Score",
    y = "Drug Offence Rate (per 100,000 population)"
  ) +
  theme_minimal()

# Correlation

cor(
  attainment_drug$Avg_Attainment8,
  attainment_drug$Drug_Rate,
  use = "complete.obs"
)






#Avg download speed vs drug off rate

# Merge internet speed and drug offence rate

internet_drug <- left_join(
  internet,
  drug_rate,
  by = "District"
)

# Scatter plot with line of best fit

ggplot(internet_drug,
       aes(x = Avg_Download,
           y = Drug_Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Download Speed vs Drug Offence Rate",
    x = "Average Download Speed (Mbps)",
    y = "Drug Offence Rate (per 100,000 population)"
  ) +
  theme_minimal()

# Pearson correlation

cor(
  internet_drug$Avg_Download,
  internet_drug$Drug_Rate,
  use = "complete.obs"
)







#avg download speed vs att score

internet_school <- left_join(
  internet,
  attainment,
  by = "District"
)

# Scatter plot

ggplot(
  internet_school,
  aes(
    x = Avg_Download,
    y = Avg_Attainment8
  )
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Average Download Speed vs Average Attainment 8 Score",
    x = "Average Download Speed (Mbps)",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

# Correlation

cor(
  internet_school$Avg_Download,
  internet_school$Avg_Attainment8,
  use = "complete.obs"
)