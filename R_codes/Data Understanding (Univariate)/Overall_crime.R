setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")
library(dplyr)
library(ggplot2)

crime_final <- read.csv(
  "crime_final.csv",
  stringsAsFactors = FALSE
)

names(crime_final)
head(crime_final)
dim(crime_final)

sapply(crime_final, class)
colnames(crime_final)
sort(unique(sub(" [0-9].*", "", crime_final$LSOA_Name)))
sum(is.na(crime_final))


#Total crimes in the two counties

crime_county_total <- crime_final %>%
  mutate(
    District = sub(" [0-9].*", "", LSOA_Name),
    County = case_when(
      District %in% c(
        "Great Yarmouth",
        "Norwich",
        "King's Lynn and West Norfolk",
        "Breckland"
      ) ~ "Norfolk",
      
      District %in% c(
        "Ipswich",
        "East Suffolk",
        "West Suffolk"
      ) ~ "Suffolk"
    )
  ) %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    Total_Crimes = n()
  )

crime_county_total


#Barchart showing Norfolk vs Suffolk total crimes

ggplot(crime_county_total, aes(x = County, y = Total_Crimes, fill = County)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = Total_Crimes),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Total Recorded Crimes: Norfolk vs Suffolk",
    x = "County",
    y = "Total Number of Crimes"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )



#All crime counts of each 7 aff districts

selected_districts <- c(
  "Great Yarmouth",
  "Ipswich",
  "Norwich",
  "King's Lynn and West Norfolk",
  "Breckland",
  "East Suffolk",
  "West Suffolk"
)

district_crime <- crime_final %>%
  mutate(
    District = sub(" [0-9].*", "", LSOA_Name)
  ) %>%
  filter(District %in% selected_districts) %>%
  group_by(District) %>%
  summarise(
    Total_Crimes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Crimes))

district_crime



#Barchart for all 7 districts
ggplot(district_crime,
       aes(x = reorder(District, Total_Crimes),
           y = Total_Crimes,
           fill = District)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Crime Count by District",
    x = "District",
    y = "Total Crime Count"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 8)
  )