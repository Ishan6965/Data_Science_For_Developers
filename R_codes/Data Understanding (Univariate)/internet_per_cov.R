setwd("D:/Data Science/240013_DataScienceForDev_cw/merged_data")

library(ggplot2)
library(dplyr)
library(tidyr)

# Read datasets
performance <- read.csv(
  "internet_perf_final.csv",
  stringsAsFactors = FALSE
)

coverage <- read.csv(
  "internet_coverage_final.csv",
  stringsAsFactors = FALSE
)

# Selected districts
districts <- c(
  "BRECKLAND",
  "KING'S LYNN AND WEST NORFOLK",
  "WEST SUFFOLK"
)

# Filter datasets
performance_selected <- performance %>%
  filter(Local_Authority %in% districts)

coverage_selected <- coverage %>%
  filter(
    laua_name.x %in% c(
      "BRECKLAND",
      "KING'S LYNN AND WEST NORFOLK",
      "WEST SUFFOLK"
    )
  )




# Select relevant download speed columns
speed_data <- performance_selected %>%
  select(
    Local_Authority,
    Avg_Download_Under10,
    Avg_Download_10_30,
    Avg_Download_30_100,
    Avg_Download_100_300,
    Avg_Download_300_900,
    Avg_Download_900Plus
  )

# Convert wide to long format
speed_long <- speed_data %>%
  tidyr::pivot_longer(
    cols = starts_with("Avg_Download"),
    names_to = "Speed_Category",
    values_to = "Average_Speed"
  )

# Bar chart
ggplot(speed_long, aes(x = Local_Authority, y = Average_Speed, fill = Speed_Category)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Download Speed by Local Authority",
    x = "Local Authority",
    y = "Average Download Speed (Mbps)",
    fill = "Speed Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Line chart
ggplot(
  speed_long,
  aes(
    x = Speed_Category,
    y = Average_Speed,
    group = Local_Authority,
    color = Local_Authority
  )
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Download Speed Distribution by District",
    x = "Download Speed Category",
    y = "Average Speed (Mbps)",
    color = "Local Authority"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Full fibre take-up 

ggplot(
  performance_selected,
  aes(
    x = Local_Authority,
    y = Full_Fibre_Takeup
  )
) +
  geom_col() +
  labs(
    title = "Full Fibre Take-up by District",
    x = "District",
    y = "Full Fibre Take-up (%)"
  ) +
  theme_minimal()


#Avg monthy data usage

ggplot(
  performance_selected,
  aes(
    x = Local_Authority,
    y = Avg_Data_Usage_GB
  )
) +
  geom_col() +
  labs(
    title = "Average Monthly Data Usage by District",
    x = "District",
    y = "Data Usage (GB)"
  ) +
  theme_minimal()

#upload speed

ggplot(performance_selected,
       aes(x = Local_Authority,
           y = Avg_Upload_100_300)) +
  geom_col() +
  labs(
    title = "Average Upload Speed (100-300 Mbps Lines)",
    x = "District",
    y = "Average Upload Speed (Mbps)"
  ) +
  theme_minimal()





#Coverage line chart

ggplot(
  coverage_plot,
  aes(
    x = Coverage_Type,
    y = Percentage,
    group = Local_Authority,
    color = Local_Authority
  )
) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(
    title = "Broadband Coverage Profile by District",
    x = "Coverage Type",
    y = "Coverage (%)",
    color = "Local Authority"
  ) +
  theme_minimal()



#full fibre coverage bar chart

ggplot(
  coverage_selected,
  aes(
    x = Local_Authority,
    y = Full_Fibre_Coverage
  )
) +
  geom_bar(
    stat = "identity"
  ) +
  labs(
    title = "Full Fibre Coverage Comparison",
    x = "Local Authority",
    y = "Full Fibre Coverage (%)"
  ) +
  theme_minimal()


#Gigabit Broadband Coverage bar chart

ggplot(
  coverage_selected,
  aes(
    x = Local_Authority,
    y = Gigabit_Coverage
  )
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Gigabit Broadband Coverage Comparison",
    x = "Local Authority",
    y = "Gigabit Coverage (%)"
  ) +
  theme_minimal()



#Ultra-Fast Broadband Coverage barchart

ggplot(
  coverage_selected,
  aes(
    x = Local_Authority,
    y = UFBB_Coverage
  )
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Ultra-Fast Broadband Coverage Comparison",
    x = "Local Authority",
    y = "UFBB Coverage (%)"
  ) +
  theme_minimal()


#Coverage gap (premises unable to receive good broadband)

ggplot(
  coverage_selected,
  aes(
    x = Local_Authority,
    y = X..of.premises.unable.to.receive.30Mbit.s.x
  )
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Premises Unable to Receive 30 Mbps",
    x = "Local Authority",
    y = "Percentage of Premises"
  ) +
  theme_minimal()


#premises with Full Fibre availability bar chart

ggplot(
  coverage_selected,
  aes(
    x = Local_Authority,
    y = Number.of.premises.with.Full.Fibre.availability.x
  )
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Premises with Full Fibre Availability",
    x = "Local Authority",
    y = "Number of Premises"
  ) +
  theme_minimal()