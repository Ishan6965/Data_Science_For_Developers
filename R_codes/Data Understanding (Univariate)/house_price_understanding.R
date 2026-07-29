setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(scales)


house <- read.csv(
  "house_final.csv",
  stringsAsFactors = FALSE
)

names(house)
dim(house)

unique(house$County)
unique(house$District)
unique(house$County[grepl("NORF|SUFF", house$County, ignore.case = TRUE)])

summary(house$Price)

#Only of two counties focus

house %>%
  filter(County %in% c("NORFOLK", "SUFFOLK")) %>%
  group_by(County) %>%
  summarise(
    Count = n(),
    Min = min(Price, na.rm = TRUE),
    Q1 = quantile(Price, 0.25, na.rm = TRUE),
    Median = median(Price, na.rm = TRUE),
    Mean = mean(Price, na.rm = TRUE),
    Q3 = quantile(Price, 0.75, na.rm = TRUE),
    Max = max(Price, na.rm = TRUE),
    SD = sd(Price, na.rm = TRUE)
  )

options(scipen = 999)

#Only districts of those two counties

house %>%
  filter(County %in% c("NORFOLK", "SUFFOLK")) %>%
  group_by(County, District) %>%
  summarise(
    Count = n(),
    Min = min(Price, na.rm = TRUE),
    Q1 = quantile(Price, 0.25, na.rm = TRUE),
    Median = median(Price, na.rm = TRUE),
    Mean = round(mean(Price, na.rm = TRUE), 0),
    Q3 = quantile(Price, 0.75, na.rm = TRUE),
    Max = max(Price, na.rm = TRUE),
    SD = round(sd(Price, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  arrange(Median)


selected_districts <- c(
  "GREAT YARMOUTH",
  "IPSWICH",
  "NORWICH",
  "KING'S LYNN AND WEST NORFOLK",
  "BRECKLAND",
  "EAST SUFFOLK",
  "WEST SUFFOLK",
  "BROADLAND",
  "NORTH NORFOLK",
  "SOUTH NORFOLK",
  "MID SUFFOLK",
  "BABERGH"
  
)
selected_districts


#Boxplot
house %>%
  filter(District %in% selected_districts) %>%
  ggplot(aes(
    x = reorder(District, Price, median),
    y = Price,
    fill = County
  )) +
  geom_boxplot() +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_replace_all(x, " ", "\n")) +
  coord_cartesian(ylim = c(0, 1000000)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )

#Histogram


#Line chart
house %>%
  mutate(Year = year(as.Date(Date))) %>%
  filter(
    County %in% c("NORFOLK", "SUFFOLK"),
    Year >= 2021,
    Year <= 2025
  ) %>%
  group_by(County, Year) %>%
  summarise(
    Average_Price = mean(Price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Year, y = Average_Price,
             color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2021:2025) +
  labs(
    title = "Average House Price by County (2021–2025)",
    x = "Year",
    y = "Average House Price (£)",
    color = "County"
  ) +
  theme_minimal()

#Histogram of all district
house %>%
  filter(District %in% selected_districts) %>%
  ggplot(aes(x = Price, fill = County)) +
  geom_histogram(
    bins = 20,
    color = "black"
  ) +
  facet_wrap(~District, scales = "free_y") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of House Prices by District",
    x = "House Price (£)",
    y = "Property count (frequency)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8)
  )



# Histogram of KING'S LYNN AND WEST NORFOLK

house %>%
  filter(District == "KING'S LYNN AND WEST NORFOLK") %>%
  ggplot(aes(x = Price)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  coord_cartesian(xlim = c(0, 5500000)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of House Prices in King's Lynn and West Norfolk",
    x = "House Price (£)",
    y = "Property count (Frequency)"
  ) +
  theme_minimal()






# Histogram of EAST SUFFOLK

house %>%
  filter(District == "EAST SUFFOLK") %>%
  ggplot(aes(x = Price)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  coord_cartesian(xlim = c(0, 5500000)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of House Prices in EAST SUFFOLK",
    x = "House Price (£)",
    y = "Property count (Frequency)"
  ) +
  theme_minimal()


# Histogram of BRECKLAND

house %>%
  filter(District == "BRECKLAND") %>%
  ggplot(aes(x = Price)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  coord_cartesian(xlim = c(0, 7500000)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of House Prices in BRECKLAND",
    x = "House Price (£)",
    y = "Property count (Frequency)"
  ) +
  theme_minimal()







# Histogram of WEST SUFFOLK

house %>%
  filter(District == "WEST SUFFOLK") %>%
  ggplot(aes(x = Price)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "black"
  ) +
  coord_cartesian(xlim = c(0, 7500000)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of House Prices in WEST SUFFOLK",
    x = "House Price (£)",
    y = "Property count (Frequency)"
  ) +
  theme_minimal()


#Broadland and South Norfolk
house %>%
  filter(District %in% c("SOUTH NORFOLK", "BROADLAND")) %>%
  ggplot(aes(x = Price, fill = District)) +
  geom_histogram(
    bins = 20,
    color = "black",
    alpha = 0.7
  ) +
  facet_wrap(~District, ncol = 2) +
  coord_cartesian(xlim = c(0, 1200000)) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of House Prices",
    x = "House Price (£)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )



