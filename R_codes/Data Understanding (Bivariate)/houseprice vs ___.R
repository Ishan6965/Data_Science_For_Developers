setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")
library(dplyr)
library(ggplot2)

house <- read.csv(
  "house_final.csv",
  stringsAsFactors = FALSE
)

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




# House price vs Download speed

selected <- c(
  "BRECKLAND",
  "GREAT YARMOUTH",
  "NORWICH",
  "IPSWICH",
  "EAST SUFFOLK",
  "WEST SUFFOLK",
  "KING'S LYNN AND WEST NORFOLK"
)

# House price (2024)
house_2024 <- house %>%
  filter(County %in% c("NORFOLK", "SUFFOLK")) %>%
  filter(format(as.Date(Date), "%Y") == "2024") %>%
  filter(District %in% selected) %>%
  group_by(District) %>%
  summarise(
    Median_House_Price = median(Price),
    .groups = "drop"
  )

# Internet speed
internet <- performance %>%
  filter(Local_Authority %in% selected) %>%
  transmute(
    District = Local_Authority,
    Avg_Download = Avg_Download_100_300
  )

# Merge
house_net <- left_join(
  house_2024,
  internet,
  by = "District"
)

# Scatter plot with line of best fit
ggplot(house_net,
       aes(x = Median_House_Price,
           y = Avg_Download)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "House Price vs Download Speed",
    x = "Median House Price (£)",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal()

# Correlation
cor(
  house_net$Median_House_Price,
  house_net$Avg_Download,
  use = "complete.obs"
)




#House price vs attantment score

# Convert Attainment 8 to numeric
ks4_final$Attainment8 <- as.numeric(ks4_final$Attainment8)

# Assign districts using postcode districts
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

# Merge with house prices
house_school <- left_join(
  house_2024,
  attainment,
  by = "District"
)

# Scatter plot with line of best fit
ggplot(house_school,
       aes(x = Median_House_Price,
           y = Avg_Attainment8)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "House Price vs Average Attainment 8 Score",
    x = "Median House Price (£)",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()

# Correlation
cor(
  house_school$Median_House_Price,
  house_school$Avg_Attainment8,
  use = "complete.obs"
)




#House price vs drug off rate

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

# Merge with house prices
house_drug <- left_join(
  house_2024,
  drug_rate,
  by = "District"
)

# Scatter plot
ggplot(house_drug,
       aes(x = Median_House_Price,
           y = Drug_Rate)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "House Price vs Drug Offence Rate",
    x = "Median House Price (£)",
    y = "Drug Offence Rate (per 100,000 population)"
  ) +
  theme_minimal()

# Correlation
cor(
  house_drug$Median_House_Price,
  house_drug$Drug_Rate,
  use = "complete.obs"
)







