setwd("D:/Data Science/240013_DataScienceForDev_cw/raw_data/Internet bandwidth/pc_performance")
setwd("D:/Data Science/240013_DataScienceForDev_cw/raw_data/Internet bandwidth/pc_coverage")

library(dplyr)

# Fixed performance
performance <- read.csv(
  "202407_fixed_performance_laua_r01.csv",
  stringsAsFactors = FALSE
)


# Residential fixed broadband coverage (optional)
fixed_coverage_res <- read.csv(
  "202407_fixed_laua_res_coverage_r01.csv",
  stringsAsFactors = FALSE
)

# Mobile coverage
mobile_coverage <- read.csv(
  "202409_mobile_coverage_laua_r01.csv",
  stringsAsFactors = FALSE
)





# Rename important columns

performance <- performance %>%
  rename(
    Avg_Download_Under10 = Avg_Download_Under_10Mbps,
    Avg_Download_10_30 = Avg_Download_10_30Mbps,
    Avg_Download_30_100 = Avg_Download_30_100Mbps,
    Avg_Download_100_300 = Avg_Download_100_300Mbps,
    Avg_Download_300_900 = Avg_Download_300_900Mbps,
    Avg_Download_900Plus = Avg_Download_900Mbps_Plus,
    
    Avg_Upload_Under10 = Avg_Upload_Under_10Mbps,
    Avg_Upload_10_30 = Avg_Upload_10_30Mbps,
    Avg_Upload_30_100 = Avg_Upload_30_100Mbps,
    Avg_Upload_100_300 = Avg_Upload_100_300Mbps,
    Avg_Upload_300_900 = Avg_Upload_300_900Mbps,
    Avg_Upload_900Plus = Avg_Upload_900Mbps_Plus,
    
    Avg_Data_Usage_GB = Monthly_Data_Usage,
    Avg_Full_Fibre_Usage_GB = Average.monthly.data.usage..GB..per.full.fibre.connection,
    Full_Fibre_Takeup = Full_Fibre_Takeup_Coverage,
    Full_Fibre_All_Premises = Full_Fibre_Takeup_All_Premises
  )
# Save cleaned dataset

write.csv(
  performance,
  "internet_perf.csv",
  row.names = FALSE
)





# Fixed broadband coverage
fixed_coverage <- read.csv(
  "202407_fixed_laua_coverage_r01.csv",
  stringsAsFactors = FALSE
)



# Merge fixed coverage with residential fixed coverage

coverage <- merge(
  fixed_coverage,
  fixed_coverage_res,
  by = "laua",
  all = TRUE
)

# Merge the result with mobile coverage

coverage <- merge(
  coverage,
  mobile_coverage,
  by = "laua",
  all = TRUE
)


#Rename some
coverage <- coverage %>%
  rename(
    LA_Code = laua,
    Local_Authority = laua_name,
    
    # All premises
    All_Premises = All.Premises.x,
    Matched_Premises = All.Matched.Premises.x,
    SFBB_Coverage = SFBB.availability....premises..x,
    UFBB100_Coverage = UFBB..100Mbit.s..availability....premises..x,
    UFBB_Coverage = UFBB.availability....premises..x,
    Full_Fibre_Coverage = Full.Fibre.availability....premises..x,
    Gigabit_Coverage = Gigabit.availability....premises..x,
    
    # Residential premises
    Residential_Premises = All.Premises.y,
    Residential_Matched = All.Matched.Premises.y,
    Residential_SFBB = SFBB.availability....premises..y,
    Residential_UFBB100 = UFBB..100Mbit.s..availability....premises..y,
    Residential_UFBB = UFBB.availability....premises..y,
    Residential_Full_Fibre = Full.Fibre.availability....premises..y,
    Residential_Gigabit = Gigabit.availability....premises..y
  )



write.csv(
  coverage,
  "internet_coverage.csv",
  row.names = FALSE
)