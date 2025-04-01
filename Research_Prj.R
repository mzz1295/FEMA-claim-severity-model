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
View(SE_df)

# To clean data and remove unwanted features for modeling 
SE_df <- SE_df %>% select(-c(locationOfContents, amountPaidOnContentsClaim, amountPaidOnIncreasedCostOfComplianceClaim,
  rateMethod, totalContentsInsuranceCoverage, primaryResidenceIndicator, 
  buildingDamageAmount, contentsDamageAmount, netContentsPaymentAmount, 
  contentsPropertyValue, disasterAssistanceCoverageRequired, floodCharacteristicsIndicator,
  floodWaterDuration, iccCoverage, netIccPaymentAmount, replacementCostBasis,
  stateOwnedIndicator, waterDepth, rentalPropertyIndicator, reportedZipCode,
  censusTract, censusBlockGroupFips, latitude, longitude, agricultureStructureIndicator,
  asOfDate, basementEnclosureCrawlspaceType, crsClassificationCode, dateOfLoss,
  elevationCertificateIndicator, elevationDifference, baseFloodElevation, houseWorship))

SE_df <- SE_df %>% select(-c(
  buildingDescriptionCode, 
  reportedCity,
  elevatedBuildingIndicator, 
  lowestAdjacentGrade,
  lowestFloorElevation,
  nonProfitIndicator,
  obstructionType,
  occupancyType,
  originalConstructionDate,
  originalNBDate,
  postFIRMConstructionIndicator,
  buildingDeductibleCode,
  smallBusinessIndicatorBuilding,
  condominiumCoverageTypeCode,
  contentsDeductibleCode,
  eventDesignationNumber,
  ficoNumber,
  nfipRatedCommunityNumber,
  nfipCommunityName,
  nonPaymentReasonContents,
  nonPaymentReasonBuilding,
  buildingReplacementCost,
  contentsReplacementCost,
  countyCode,
  id
))
SE_df <- SE_df %>% select(-c(
  ratedFloodZone,
  nfipCommunityNumberCurrent
))

#Intial look at data set 
summary(SE_df) 

#Large number of Nas in buildingpropertyvalue and amount paid on buildings claims
#amount paid on building claims NAs are really 0, so will convert NAs in this column as 0
library(dplyr)
SE_df <- SE_df %>% mutate(amountPaidOnBuildingClaim = ifelse(
  is.na(amountPaidOnBuildingClaim), 0, amountPaidOnBuildingClaim))
SE_df <- SE_df %>% select(-c(
  netBuildingPaymentAmount
))

#To handle missing values in buildingpropertyvalue 
#Will fill values using median 
SE_df$buildingPropertyValue[is.na(SE_df$buildingPropertyValue)] <- median(SE_df$buildingPropertyValue, na.rm=TRUE)
# To remove negative values in property values 
SE_df <- SE_df[SE_df$buildingPropertyValue > 0, ]
# Exclude 0 claims paid 
SE_df <- SE_df[SE_df$amountPaidOnBuildingClaim > 0,]
# Now to look at distribution of amount paid in claims 
library(ggplot2)
ggplot(data=SE_df, aes(x = amountPaidOnBuildingClaim))+ # Eurika a right taled distribution
  geom_histogram(fill="lightblue")+
  xlim(0,1.0e06)+
  ylim(0,1e05)
# Need to remove extreme outliers 
library(dplyr)
SE_df$amountPaidOnBuildingClaim = replace(
  SE_df$amountPaidOnBuildingClaim, SE_df$amountPaidOnBuildingClaim > quantile(SE_df$amountPaidOnBuildingClaim, .99), NA)
# Now to look at dsitribution 
library(ggplot2)
ggplot(data=SE_df, aes(x = amountPaidOnBuildingClaim))+ # Eurika a right taled distribution
  geom_histogram(fill="lightblue")+
  xlim(0,1.0e06)+
  ylim(0,1e05)
# To assess categorical data 
# TO assess flood zone
library(ggplot2)
ggplot(data=SE_df, aes(x=floodZoneCurrent, fill=floodZoneCurrent, color=floodZoneCurrent))+
  geom_bar(position="identity", alpha=0.5)
# Data has high variability with majority NAs 
# Will drop this column
library(dplyr)
SE_df <- SE_df %>% select(-c(
  floodZoneCurrent
))
# Now to look at flood event 
library(ggplot2)
ggplot(data=SE_df, aes(x=floodEvent, fill=floodEvent, color=floodEvent))+
  geom_bar(position="identity", alpha=0.5)
# data looks normally distrbuted 
# Will be log transforming the claim cost target feature, so this is fine 
# Wil fill NAs with mode 

summary(SE_df)

library(dplyr)

SE_df <- SE_df%>%
  mutate(floodEvent=if_else(is.na(floodEvent), "Hurricane Helene", floodEvent))

# Now to look at distribution 
library(ggplot2)
ggplot(data=SE_df, aes(x=floodEvent, fill=floodEvent, color=floodEvent))+
  geom_bar(position="identity", alpha=0.5)
# Looks fairly normal when replaced with mode, Hurricane Helen
# Quick look at distribution of data to state 
library(ggplot2)
ggplot(data=SE_df, aes(x=state, fill=state, color=state))+
  geom_bar(position="identity", alpha=0.5)
# Florida is the majority count of claims 
# Now we need to look at data types and see what may be wrong 
str(SE_df) # Cause of damage is a character not int type 
SE_df$causeOfDamage <- as.numeric(SE_df$causeOfDamage)
str(SE_df)
### Now to add average wind speed based on hurricane as a new column 
library(dplyr)
SE_df <- SE_df %>%
  mutate(max_wind_speed=case_when(
    floodEvent == "April Florida Flooding" ~ 26.5,
    floodEvent == "Blizzard of 1993" ~ 110,
    floodEvent == "December Nor'easter" ~ 39,
    floodEvent == 'Early winter storms' ~ 53,
    floodEvent == 'Flooding' ~ 19,
    floodEvent == 'Fort Lauderdale Flooding' ~ 35,
    floodEvent == 'Hurricane Alex' ~ 70,
    floodEvent == 'Hurricane Andrew' ~ 165,
    floodEvent == 'Hurricane Bertha' ~ 105,
    floodEvent == 'Hurricane Bonnie' ~ 85,
    floodEvent == 'Hurricane Charley' ~ 150,
    floodEvent == 'Hurricane Cindy' ~ 75,
    floodEvent == 'Hurricane Debby' ~ 80,
    floodEvent == 'Hurricane Dennis' ~ 150,
    floodEvent == 'Hurricane Diana' ~ 130,
    floodEvent == 'Hurricane Earl' ~ 110,
    floodEvent == 'Hurricane Elana' ~ 125,
    floodEvent == 'Hurricane Elsa' ~ 85,
    floodEvent == 'Hurricane Emily' ~ 160,
    floodEvent == 'Hurricane Eta' ~ 150,
    floodEvent == 'Hurricane Florence' ~ 150,
    floodEvent == 'Hurricane Floyd' ~ 155,
    floodEvent == 'HUrricane Fran' ~ 120,
    floodEvent == 'Hurricane Frances' ~ 145,
    floodEvent == 'Hurricane Francine' ~ 100,
    floodEvent == 'Hurricane Georges (Keys)' ~ 105,
    floodEvent == 'Hurricane Georges (Panhandle)' ~ 65,
    floodEvent == 'Hurricane Gloria' ~ 145,
    floodEvent == 'Hurricane Gordon' ~ 67,
    floodEvent == 'Hurricane Hanna' ~ 85,
    floodEvent == 'Hurricane Helene' ~ 140,
    floodEvent == 'Hurricane Hermine' ~ 80,
    floodEvent == 'Hurricane Hugo' ~ 138,
    floodEvent == 'Hurricane Ian' ~ 155,
    floodEvent == 'Hurricane Idalia' ~ 134,
    floodEvent == 'Hurricane Ike' ~ 110,
    floodEvent == 'Hurricane Irene' ~ 115,
    floodEvent == 'Hurricane Irma' ~ 142,
    floodEvent == 'Hurricane Issac' ~ 80,
    floodEvent == 'Hurricane Isabel' ~ 165,
    floodEvent == 'Hurricane Isaias' ~ 85,
    floodEvent == 'Hurricane Ivan' ~ 120,
    floodEvent == 'Hurricane Jeanne' ~ 120,
    floodEvent == 'Hurricane Josephine' ~ 105,
    floodEvent == 'Hurricane Kate' ~ 120,
    floodEvent == 'Hurricane Katrina' ~ 175,
    floodEvent == 'Hurricane Matthew' ~ 120,
    floodEvent == 'Hurricane Michael' ~ 160,
    floodEvent == 'Hurricane Milton' ~ 180,
    floodEvent == 'Hurricane Nicole' ~ 75,
    floodEvent == 'Hurricane Opal' ~ 115,
    floodEvent =='Hurricane Ophelia' ~ 70,
    floodEvent == 'Hurricane Sally' ~ 103,
    floodEvent =='Hurricane Sandy' ~ 75,
    floodEvent == 'Hurricane Wilma' ~ 185,
    floodEvent == 'Hurricane Zeta' ~ 110,
    floodEvent == 'June South Florida Flooding' ~ 14,
    floodEvent == 'Late summer storms' ~ 58,
    floodEvent == 'Low pressure system' ~ 20,
    floodEvent == 'Mid-spring storms' ~ 14,
    floodEvent == 'October severe storms' ~ 80,
    floodEvent == 'Sever Storms and Flooding' ~ 60,
    floodEvent == "The 'No-Name Storm'" ~ 39,
    floodEvent == 'The "Halloween" Storm' ~ 18,
    floodEvent == 'Torrential rain' ~ 100,
    floodEvent == 'Tropical Cyclone Eight' ~ 45,
    floodEvent == 'Tropical Storm Alberto' ~ 50,
    floodEvent == 'Tropical Storm Alex' ~ 70,
    floodEvent == 'Tropical Storm Allison' ~ 60,
    floodEvent == 'Tropical Storm Barry' ~ 75,
    floodEvent == 'Tropical Storm Claudette' ~ 81,
    floodEvent == 'Tropical Storm Debby' ~ 80,
    floodEvent == 'Tropical Storm Ernesto' ~ 60,
    floodEvent == 'Tropical Storm Fay' ~ 65,
    floodEvent == 'Tropical Storm Fred' ~ 65,
    floodEvent == 'Tropical Storm Isidore' ~ 127,
    floodEvent == 'Tropical Storm Ivan' ~ 168,
    floodEvent == 'Tropical Storm Mitch' ~ 180,
    floodEvent == 'Tropical Storm Nicholas' ~ 75,
    floodEvent == 'Tropical Storm Ophelia' ~ 70
  )) 
str(SE_df)
# Distribution of cause of damage 
library(ggplot2)
ggplot(SE_df, aes(x = causeOfDamage))+
  geom_density(fill="lightblue", alpha = 0.5)+
  theme_minimal()
# Same for number of floors
library(ggplot2)
ggplot(SE_df, aes(x = numberOfFloorsInTheInsuredBuilding))+
  geom_density(fill="lightblue", alpha = 0.5)+
  theme_minimal()+
  xlim(-1,7)
# totalBuildingInsurancecoverage
library(ggplot2)
ggplot(SE_df, aes(x = totalBuildingInsuranceCoverage))+
  geom_density(fill="lightblue", alpha = 0.5)+
  theme_minimal()+
  xlim()
# fairly normal 
# Count of number of units
install.packages("tidyverse")
library(tidyverse)
SE_df %>% count(numberOfUnits)
# Same for number of floors 
library(tidyverse)
SE_df %>% count(numberOfFloorsInTheInsuredBuilding)
# Both are left tailed looking like gamma distributions
# Correlation plot of numeric variables, particularly to target variable 

#######################################################
### Time to create the model
#######################################################


  
  
  
