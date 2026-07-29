setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")

library(dplyr)
library(ggplot2)


# Load KS4 dataset
ks4_final <- read.csv("ks4_final.csv")


# Convert Attainment8 to numeric
ks4_final$Attainment8 <- as.numeric(ks4_final$Attainment8)



# Select target districts using postcode districts
ks4_selected <- ks4_final %>%
  mutate(
    Postcode_District = sub(" .*", "", Postcode),
    
    District = case_when(
      Postcode_District %in% c(
        "NR17","NR19","NR20","IP24"
      ) ~ "Breckland",
      
      Postcode_District %in% c(
        "PE30","PE31","PE32","PE33","PE34","PE35"
      ) ~ "King's Lynn and West Norfolk",
      
      Postcode_District %in% c(
        "IP27","IP28","IP29","IP30","IP31","IP32","IP33"
      ) ~ "West Suffolk"
    )
  ) %>%
  filter(!is.na(District),
         !is.na(Attainment8))



# Check number of schools
ks4_selected %>%
  count(District)




# -------------------------------
# Boxplot: Attainment 8 distribution
# -------------------------------

ggplot(ks4_selected,
       aes(x = District,
           y = Attainment8)) +
  geom_boxplot() +
  labs(
    title = "Attainment 8 Score Distribution (2024)",
    x = "District",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# -------------------------------
# Bar chart: Average Attainment 8
# -------------------------------

ggplot(avg_attainment,
       aes(x = District,
           y = Average_Attainment8)) +
  geom_col() +
  labs(
    title = "Average Attainment 8 Score by District (2024)",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# -----------------------------
# Line chart: Average Attainment8 comparison
# -----------------------------

ggplot(avg_attainment,
       aes(x = District,
           y = Average_Attainment8,
           group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Average Attainment 8 Score Trend Across Districts (2024)",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal()



class(ks4_selected$Attainment8)

summary(ks4_selected$Attainment8)


sum(!is.na(ks4_selected$Attainment8))