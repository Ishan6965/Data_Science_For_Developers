setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("fmsb")
library(fmsb)

population_final <- read.csv(
  "population_2024.csv",
  stringsAsFactors = FALSE
)
crime_final <- read.csv(
  "crime_final.csv",
  stringsAsFactors = FALSE
)
names(population_final)


popn_crime <- crime_final %>%
  left_join(
    population_final %>%
      select(LSOA_Name, LAD_Name, Population),
    by = "LSOA_Name"
  )


write.csv(
  popn_crime,
  "crime_popn.csv",
  row.names = FALSE
)


names(popn_crime)


crime_popn <- read.csv(
  "crime_popn.csv",
  stringsAsFactors = FALSE
)

# Vehicle, drug off, violence/sexual off and robbery rate per 100000 for Nov 2024

lad_population <- population_final %>%
  group_by(LAD_Name) %>%
  summarise(
    Population = sum(Population),
    .groups = "drop"
  )

# November 2024 crime rates

crime_rate_nov2024 <- crime_popn %>%
  filter(
    Month == "2024-11-01",
    Crime_Type %in% c(
      "Vehicle crime",
      "Robbery",
      "Drugs",
      "Violence and sexual offences"
    ),
    LAD_Name %in% c(
      "Great Yarmouth",
      "King's Lynn and West Norfolk",
      "Breckland",
      "West Suffolk"
    )
  ) %>%
  group_by(LAD_Name, Crime_Type) %>%
  summarise(
    Crime_Count = n(),
    .groups = "drop"
  ) %>%
  left_join(lad_population, by = "LAD_Name") %>%
  mutate(
    Crime_Rate_per_100k = (Crime_Count / Population) * 100000
  )


------------------------------------------
#statistics of all 4 districs and 4 crimes
------------------------------------------
crime_rate_nov2024 %>%
  group_by(Crime_Type) %>%
  summarise(
    Min = min(Crime_Rate_per_100k),
    Q1 = quantile(Crime_Rate_per_100k, 0.25),
    Median = median(Crime_Rate_per_100k),
    Mean = mean(Crime_Rate_per_100k),
    Q3 = quantile(Crime_Rate_per_100k, 0.75),
    Max = max(Crime_Rate_per_100k),
    SD = sd(Crime_Rate_per_100k),
    .groups = "drop"
  )







---------------------------------
# Line Chart for Drug Crime Rate
---------------------------------
  
drug <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Drugs")

ggplot(
  drug,
  aes(
    x = reorder(LAD_Name, Crime_Rate_per_100k),
    y = Crime_Rate_per_100k,
    group = 1
  )
) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Drug Crime Rate per 100,000 Population (November 2024)",
    x = "District",
    y = "Crime Rate per 100,000"
  ) +
  theme_minimal()

--------------------------------
# Bar Chart for Drug Crime Rate
--------------------------------
  
drug <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Drugs")

ggplot(
  drug,
  aes(
    x = reorder(LAD_Name, Crime_Rate_per_100k),
    y = Crime_Rate_per_100k,
    fill = LAD_Name
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(Crime_Rate_per_100k, 1)),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Drug Crime Rate per 100,000 Population (November 2024)",
    x = "District",
    y = "Crime Rate per 100,000"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )




#----------------------------------------------------
# Radar Chart for Vehicle Crime Rate
#----------------------------------------------------

vehicle <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Vehicle crime") %>%
  select(LAD_Name, Crime_Rate_per_100k)

radar_vehicle <- as.data.frame(rbind(
  max = rep(max(vehicle$Crime_Rate_per_100k), nrow(vehicle)),
  min = rep(0, nrow(vehicle)),
  value = vehicle$Crime_Rate_per_100k
))

colnames(radar_vehicle) <- vehicle$LAD_Name

radarchart(
  radar_vehicle,
  axistype = 1,
  pcol = "blue",
  pfcol = rgb(0, 0, 1, 0.3),
  plwd = 2,
  title = "Vehicle Crime Rate per 100,000 (November 2024)"
)


-----------------------------------
# Bar Chart for Vehicle Crime Rate
-----------------------------------
vehicle <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Vehicle crime")

ggplot(
  vehicle,
  aes(
    x = reorder(LAD_Name, Crime_Rate_per_100k),
    y = Crime_Rate_per_100k,
    fill = LAD_Name
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(Crime_Rate_per_100k, 1)),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Vehicle Crime Rate per 100,000 Population (November 2024)",
    x = "District",
    y = "Crime Rate per 100,000"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )



#----------------------------------------------------
# Labelled Pie Chart for Robbery Rate
#----------------------------------------------------

robbery <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Robbery") %>%
  complete(
    LAD_Name = c(
      "Breckland",
      "Great Yarmouth",
      "King's Lynn and West Norfolk",
      "West Suffolk"
    ),
    fill = list(
      Crime_Count = 0,
      Crime_Rate_per_100k = 0
    )
  )

ggplot(
  robbery,
  aes(
    x = "",
    y = Crime_Rate_per_100k,
    fill = LAD_Name
  )
) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(
      label = paste0(
        LAD_Name,
        "\n",
        round(Crime_Rate_per_100k, 1)
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 4
  ) +
  labs(
    title = "Robbery Rate per 100,000 Population (November 2024)"
  ) +
  theme_void()

#----------------------------------------------------
# Radar Chart for Violence and Sexual Offences
#----------------------------------------------------

violence <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Violence and sexual offences") %>%
  select(LAD_Name, Crime_Rate_per_100k)

radar_violence <- as.data.frame(rbind(
  max = rep(max(violence$Crime_Rate_per_100k), nrow(violence)),
  min = rep(0, nrow(violence)),
  value = violence$Crime_Rate_per_100k
))

colnames(radar_violence) <- violence$LAD_Name

radarchart(
  radar_violence,
  axistype = 1,
  pcol = "red",
  pfcol = rgb(1, 0, 0, 0.3),
  plwd = 2,
  title = "Violence & Sexual Offence Rate per 100,000 (November 2024)"
)

--------------------------------------------------
# Bar Chart for Violence and Sexual Offences Rate
--------------------------------------------------
  
violence <- crime_rate_nov2024 %>%
  filter(Crime_Type == "Violence and sexual offences")

ggplot(
  violence,
  aes(
    x = reorder(LAD_Name, Crime_Rate_per_100k),
    y = Crime_Rate_per_100k,
    fill = LAD_Name
  )
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = round(Crime_Rate_per_100k, 1)),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Violence and Sexual Offences Rate per 100,000 Population (November 2024)",
    x = "District",
    y = "Crime Rate per 100,000"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )