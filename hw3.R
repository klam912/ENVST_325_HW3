library(ggplot2)
library(dplyr)
library(lubridate)

# Download datasets
climate_change <- read.csv("./activity03/climate-change.csv")
annual_co_region <- read.csv("./activity03/annual-co-emissions-by-region.csv")

# In-class activities ----
# Replace the 4th column name 
colnames(annual_co_region)[4] <- "CO2"

# Reassign Entity into a factor type
annual_co_region$Entity <- as.factor(annual_co_region$Entity)

# Filter out US ddata
US <- annual_co_region %>%
  filter(Entity == "United States")

# Plot CO2 by year for the U.S.
plot(US$Year, US$CO2, type="b",
     pch=19,
     xlab="Year",
     ylab="Fossil fuel emissions (billions of tons of CO2",
     yaxt="n") # Turn off the y axis

# Make custom y axis labes
axis(2, seq(0, 6000000000, by=2000000000),
     seq(0,6, by=2),
     las=2) # scale down y axis units 

ggplot(US, aes(x=Year, y=CO2)) +
  geom_point() +
  geom_line() +
  labs(
    x="Year",
    y="Fossil fuel emissions (billions of tons of CO2)",
    title="Fossil fuel emission by the year"
  ) +
  theme_classic()

# Filter North American countries
NorthA <- annual_co_region %>%
  filter(
    Entity == "United States" | Entity == "Mexico" | Entity == "Canada"
  )

# Plot CO2 emission by year for NA
ggplot(NorthA, aes(x=Year, y=CO2, color=Entity)) +
  geom_point() +
  geom_line() + 
  scale_color_manual(values=c("red", "royalblue", "darkgoldenrod3"))


# In-class prompts ----
# Prompt 1
# Lubridate the year
climate_change$Day <- ymd(climate_change$Day)

# Reassign Entity as factor
climate_change$Entity <- as.factor(climate_change$Entity)

# Using base r
north_and_southern_hemispheres <- climate_change %>% filter(Entity == "Northern Hemisphere" | Entity == "Southern Hemisphere")

# Reassign Entity as factor
north_and_southern_hemispheres$Entity <- as.factor(north_and_southern_hemispheres$Entity)

plot(north_and_southern_hemispheres$Day,
     north_and_southern_hemispheres$temperature_anomaly,
     col=north_and_southern_hemispheres$Entity,
     pch=19,
     xlab= "Year",
     ylab = "Temperature anomaly (Celsius)")

# Add a legend
legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

# Using ggplot2
ggplot(data=north_and_southern_hemispheres, aes(x=Day, y=temperature_anomaly, color=Entity)) +
  geom_point() +
  labs(
    y = "Temperature anomaly (Celsius)",
    x = "Year",
    title = "Histogram of temperature anomalies between Northern and Southern Hemisphere"
  ) 

# Prompt 2
# Plot the total all time emissions for the United States, Mexico, and Canada.
americas <- annual_co_region %>% 
  filter(Entity == "United States" | Entity == "Mexico" | Entity == "Canada")

# Sum up all emissions for the 3 countries
all_time_emissions_Americas <- americas %>%
  group_by(Entity) %>%
  summarise(total_emission = sum(CO2))

ggplot(all_time_emissions_Americas, aes(x=Entity, y=total_emission / 1e9)) +
  geom_bar(stat = "identity") +
  labs(
    x="Country",
    y="Total CO2 Emission (billions of tons in CO2)",
    title="Total All Time Emissions for US, Mexico, and Canada"
  )
