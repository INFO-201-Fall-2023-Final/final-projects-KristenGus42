library(dplyr)
library(ggplot2)

alcohol_df <- read.csv("consumption.csv")
life_df <- read.csv("life_expectancy.csv")

# Merging the datasets on 'Country' and 'Year'
merged_df <- merge(life_df, alcohol_df, by = c("Country", "Year"), na.rm = TRUE)

# Handling cases where 'Alcohol' data is NA in 'life_df'
merged_df$Alcohol[is.na(merged_df$Alcohol)] <- 0

# Creating a new column 'Average_Alcohol_Consumption' that averages the values
merged_df$Average_Alcohol_Consumption <- rowMeans(merged_df[, c("Alcohol", "Alcohol.total.per.capita..15...consumption.in.liters..numeric.")], na.rm = TRUE)

# According to WHO, 5.5 litres is the average amount of alcohol consumption per capita. 
# BELOW AVERAGE = CONSUMPTION =< 5.5 
# ABOVE AVERAGE = CONSUMPTION > 5.5
merged_df$Consumption_Comparison <- ifelse(merged_df$Average_Alcohol_Consumption > 5.5, "ABOVE AVERAGE", "BELOW AVERAGE")

#Summarizing into drinking for females in 2015
merged_2015 <- filter(merged_df, Year == "2015")
merged_female <- filter(merged_2015, Sex == "Female")

merged_avg <- summarize(
  group_by(merged_female, Country),
  mean_alcohol = mean(Average_Alcohol_Consumption),
  mean_expectancy = mean(Life.expectancy),
  mean_percentage = mean(percentage.expenditure)
)



bar_graph <- ggplot(merged_df) +
  geom_col(mapping = aes(x = Status, y = Average_Alcohol_Consumption, fill="pink")) +
  labs(x = "Status of Country", y = "Average Alcohol Consumption in Liters")

plot(bar_graph)

