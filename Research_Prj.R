df <- read.csv("FimaNfipClaims.csv")
df[df == "" | df == " "] <- NA ### To add NA to any blank observations
View(df)
FL_df <- subset(df, state %in% c("FL")) ### To create subset of only FL data 
View(FL_df)
NC_df <- subset(df, state %in% c("NC")) ### North Carolina 
View(NC_df)
GA_df <- subset(df, state %in% c("GA")) ### Georgia
View(GA_df)
SC_df <- subset(df, state %in% c("SC")) ### South Carolina
View(SC_df)

### To remove unwanted features 
library(dplyr)
### Florida 
FL_df <- FL_df %>% select(-c(agricultureStructureIndicator, asOfDate, basementEnclosureCrawlspaceType, crsClassificationCode, elevatedBuildingIndicator, dateOfLoss, elevationCertificateIndicator, elevationDifference, baseFloodElevation, houseWorship, lowestAdjacentGrade, lowestFloorElevation, nonProfitIndicator, obstructionType, occupancyType, originalConstructionDate, originalNBDate, postFIRMConstructionIndicator, smallBusinessIndicatorBuilding, buildingDeductibleCode, condominiumCoverageTypeCode, eventDesignationNumber, ficoNumber, nfipRatedCommunityNumber, nfipCommunityNumberCurrent, nfipCommunityName, nonPaymentReasonContents, nonPaymentReasonBuilding))
FL_df <- FL_df %>% select(-c(numberOfUnits, replacementCostBasis, stateOwnedIndicator, rentalPropertyIndicator, reportedCity, censusTract, censusBlockGroupFips, id))
FL_df <- FL_df %>% select(-c(contentsDeductibleCode, buildingReplacementCost, contentsReplacementCost, countyCode))
View(FL_df)
### North Carolina
NC_df <- NC_df %>% select(-c(agricultureStructureIndicator, asOfDate, basementEnclosureCrawlspaceType, crsClassificationCode, elevatedBuildingIndicator, dateOfLoss, elevationCertificateIndicator, elevationDifference, baseFloodElevation, houseWorship, lowestAdjacentGrade, lowestFloorElevation, nonProfitIndicator, obstructionType, occupancyType, originalConstructionDate, originalNBDate, postFIRMConstructionIndicator, smallBusinessIndicatorBuilding, buildingDeductibleCode, condominiumCoverageTypeCode, eventDesignationNumber, ficoNumber, nfipRatedCommunityNumber, nfipCommunityNumberCurrent, nfipCommunityName, nonPaymentReasonContents, nonPaymentReasonBuilding))
NC_df <- NC_df %>% select(-c(numberOfUnits, replacementCostBasis, stateOwnedIndicator, rentalPropertyIndicator, reportedCity, censusTract, censusBlockGroupFips, id))
NC_df <- NC_df %>% select(-c(contentsDeductibleCode, buildingReplacementCost, contentsReplacementCost, countyCode))
View(NC_df)
### Georgia
GA_df <- GA_df %>% select(-c(agricultureStructureIndicator, asOfDate, basementEnclosureCrawlspaceType, crsClassificationCode, elevatedBuildingIndicator, dateOfLoss, elevationCertificateIndicator, elevationDifference, baseFloodElevation, houseWorship, lowestAdjacentGrade, lowestFloorElevation, nonProfitIndicator, obstructionType, occupancyType, originalConstructionDate, originalNBDate, postFIRMConstructionIndicator, smallBusinessIndicatorBuilding, buildingDeductibleCode, condominiumCoverageTypeCode, eventDesignationNumber, ficoNumber, nfipRatedCommunityNumber, nfipCommunityNumberCurrent, nfipCommunityName, nonPaymentReasonContents, nonPaymentReasonBuilding))
GA_df <- GA_df %>% select(-c(numberOfUnits, replacementCostBasis, stateOwnedIndicator, rentalPropertyIndicator, reportedCity, censusTract, censusBlockGroupFips, id))
GA_df <- GA_df %>% select(-c(contentsDeductibleCode, buildingReplacementCost, contentsReplacementCost, countyCode))
View(GA_df)
### South Carolina 
SC_df <- SC_df %>% select(-c(agricultureStructureIndicator, asOfDate, basementEnclosureCrawlspaceType, crsClassificationCode, elevatedBuildingIndicator, dateOfLoss, elevationCertificateIndicator, elevationDifference, baseFloodElevation, houseWorship, lowestAdjacentGrade, lowestFloorElevation, nonProfitIndicator, obstructionType, occupancyType, originalConstructionDate, originalNBDate, postFIRMConstructionIndicator, smallBusinessIndicatorBuilding, buildingDeductibleCode, condominiumCoverageTypeCode, eventDesignationNumber, ficoNumber, nfipRatedCommunityNumber, nfipCommunityNumberCurrent, nfipCommunityName, nonPaymentReasonContents, nonPaymentReasonBuilding))
SC_df <- SC_df %>% select(-c(numberOfUnits, replacementCostBasis, stateOwnedIndicator, rentalPropertyIndicator, reportedCity, censusTract, censusBlockGroupFips, id))
SC_df <- SC_df %>% select(-c(contentsDeductibleCode, buildingReplacementCost, contentsReplacementCost, countyCode))
View(SC_df)

## write.csv(GA_df, "GA_data.csv") was just to show the output 

##########################################
### Property Damage Analysis 
##########################################

### To sample the data for average property damage by every 5 years 
library(dplyr)

### Florida Average Property Damage Visual

FL_average_damage1 <- FL_df %>% filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% group_by(yearOfLoss) %>% summarise(avg_damage = mean(buildingDamageAmount, na.rm = TRUE) )
View(FL_average_damage1)### Years 2000 to 2024

### To create scatter plot for the damage over the years 
library(ggplot2)
ggplot(FL_average_damage1, aes (x = yearOfLoss, y = avg_damage)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "darkgrey", linetype = "dashed") +
  labs(
    title = "Florida Average Property Damage (2000 - 2024)",
    x = "Year",
    y = "Property Damage Amount (in Dollars)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = "14"),
    axis.title = element_text(size = 12)
  )
### North Carolina Average Property Damage Visual 

NC_average_damage1 <- NC_df %>% filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% group_by(yearOfLoss) %>% summarise(avg_damage = mean(buildingDamageAmount, na.rm = TRUE) )  
View(NC_average_damage1)

### Visual 
library(ggplot2)
ggplot(NC_average_damage1, aes (x = yearOfLoss, y = avg_damage)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "darkgrey", linetype = "dashed") +
  labs(
    title = "North Carolina Average Property Damage (2000 - 2024)",
    x = "Year",
    y = "Property Damage Amount (in Dollars)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = "14"),
    axis.title = element_text(size = 12)
  )

### Georgia Average Property Damage Visual 
GA_average_damage1 <- GA_df %>% filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% group_by(yearOfLoss) %>% summarise(avg_damage = mean(buildingDamageAmount, na.rm = TRUE) )
View(GA_average_damage1)

### Visual 
library(ggplot2)
ggplot(GA_average_damage1, aes (x = yearOfLoss, y = avg_damage)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "darkgrey", linetype = "dashed") +
  labs(
    title = "Georgia Average Property Damage (2000 - 2024)",
    x = "Year",
    y = "Property Damage Amount (in Dollars)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = "14"),
    axis.title = element_text(size = 12)
  )

### South Carolina average property damage 
SC_average_damage1 <- SC_df %>% filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% group_by(yearOfLoss) %>% summarise(avg_damage = mean(buildingDamageAmount, na.rm = TRUE) )
View(SC_average_damage1)
### Visual 
library(ggplot2)
ggplot(SC_average_damage1, aes (x = yearOfLoss, y = avg_damage)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "darkgrey", linetype = "dashed") +
  labs(
    title = "South Carolina Average Property Damage (2000 - 2024)",
    x = "Year",
    y = "Property Damage Amount (in Dollars)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = "14"),
    axis.title = element_text(size = 12)
  )


###############################################
### Property Evaluation Analysis  
###############################################

### To find percent change in average property value in FL 
FL_property_value_change <- FL_df %>% 
  filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% 
  group_by(yearOfLoss) %>%
  summarise(avg_property_value_FL = mean(buildingPropertyValue, na.rm = TRUE)) %>%
  arrange(yearOfLoss) %>% 
  mutate(percent_change_FL = (avg_property_value_FL - lag(avg_property_value_FL)) / lag(avg_property_value_FL) * 100)
FL_property_value_change <- FL_property_value_change %>% filter(!is.na(percent_change_FL))
View(FL_property_value_change)

### Visual FL
install.packages("plotly")
library(ggplot2)
library(plotly)
FL_plotly <- ggplot(FL_property_value_change, aes(x = factor(yearOfLoss), y = percent_change_FL, text = sprintf("Year: %d\nChange: %.2f%%", yearOfLoss, percent_change_FL))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "FL Yearly Percent Change in Average Property Value (2000 - 2024)",
       x = "Year of Loss",
       y = "Percent Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interactive_plot_FL <- ggplotly(FL_plotly, tooltip = "text")
interactive_plot_FL

### Percent Change for North Carolina
NC_property_value_change <- NC_df %>% 
  filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% 
  group_by(yearOfLoss) %>%
  summarise(avg_property_value_NC = mean(buildingPropertyValue, na.rm = TRUE)) %>%
  arrange(yearOfLoss) %>% 
  mutate(percent_change_NC = (avg_property_value_NC - lag(avg_property_value_NC)) / lag(avg_property_value_NC) * 100)
NC_property_value_change <- NC_property_value_change %>% filter(!is.na(percent_change_NC))
View(NC_property_value_change)

### Visual NC 
library(ggplot2)
library(plotly)
NC_plotly <- ggplot(NC_property_value_change, aes(x = factor(yearOfLoss), y = percent_change_NC, text = sprintf("Year: %d\nChange: %.2f%%", yearOfLoss, percent_change_NC))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "NC Yearly Percent Change in Average Property Value (2000 - 2024)",
       x = "Year of Loss",
       y = "Percent Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interactive_plot_NC <- ggplotly(NC_plotly, tooltip = "text")
interactive_plot_NC

### Percent Change for GA
GA_property_value_change <- GA_df %>% 
  filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% 
  group_by(yearOfLoss) %>%
  summarise(avg_property_value_GA = mean(buildingPropertyValue, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(yearOfLoss) %>% 
  mutate(percent_change_GA = (avg_property_value_GA - lag(avg_property_value_GA)) / lag(avg_property_value_GA) * 100)
  filter(!is.na(lag(avg_property_value_GA)))
View(GA_property_value_change)

### Visual GA
library(ggplot2)
library(plotly)
GA_plotly <- ggplot(GA_property_value_change, aes(x = factor(yearOfLoss), y = percent_change_GA, text = sprintf("Year: %d\nChange: %.2f%%", yearOfLoss, percent_change_GA))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "GA Yearly Percent Change in Average Property Value (2000 - 2024)",
       x = "Year of Loss",
       y = "Percent Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interactive_plot_GA <- ggplotly(GA_plotly, tooltip = "text")
interactive_plot_GA

### Percent Change in SC
SC_property_value_change <- SC_df %>% 
  filter(yearOfLoss >= 2000 & yearOfLoss <= 2024) %>% 
  group_by(yearOfLoss) %>%
  summarise(avg_property_value_SC = mean(buildingPropertyValue, na.rm = TRUE)) %>%
  arrange(yearOfLoss) %>% 
  mutate(percent_change_SC = (avg_property_value_SC - lag(avg_property_value_SC)) / lag(avg_property_value_SC) * 100)
SC_property_value_change <- SC_property_value_change %>% filter(!is.na(percent_change_SC))
View(SC_property_value_change)

### Visual SC
library(ggplot2)
library(plotly)
SC_plotly <- ggplot(SC_property_value_change, aes(x = factor(yearOfLoss), y = percent_change_SC, text = sprintf("Year: %d\nChange: %.2f%%", yearOfLoss, percent_change_SC))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "SC Yearly Percent Change in Average Property Value (2000 - 2024)",
       x = "Year of Loss",
       y = "Percent Change (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

interactive_plot_SC <- ggplotly(SC_plotly, tooltip = "text")
interactive_plot_SC
############################################
### Interactive Map Showing Claims Paid
############################################

# Packages
install.packages("leaflet")
install.packages("dplyr")
install.packages("scales")

# Load Libraries
library(leaflet)
library(dplyr)
library(scales)

# To create data frame with data 
library(dplyr)
map_data_SE <- bind_rows(
  FL_df %>% select(amountPaidOnBuildingClaim, longitude, latitude),
  NC_df %>% select(amountPaidOnBuildingClaim, longitude, latitude),
  SC_df %>% select(amountPaidOnBuildingClaim, longitude, latitude),
  GA_df %>% select(amountPaidOnBuildingClaim, longitude, latitude)
)
map_data_SE <- na.omit(map_data_SE)
View(map_data_SE)
# To create interactive map
install.packages("leaflet.markercluster")
library(leaflet)
library(leaflet.extras)

# Leaflet map   
leaflet(map_data_SE) %>%
  addTiles() %>% 
  addCircleMarkers(
    ~longitude, ~latitude, 
    color = "blue", 
    fillOpacity = 0.5,
    radius = 6,
    popup = ~paste("Amount Paid: $", amountPaidOnBuildingClaim),
    clusterOptions = markerClusterOptions() 
  )

###########################################################
### To create a df with all southeast states for modeling 
###########################################################

# To bind all dfs 
library(dplyr)
library(sp)
SE_df <- bind_rows(FL_df, SC_df, NC_df, GA_df)

# To clean data and remove unwanted features for modeling 


