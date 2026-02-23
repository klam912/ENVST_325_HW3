library(ggplot2)
library(dplyr)
library(lubridate)
library(viridis)

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
    title="Total All Time Emissions for US, Mexico, and Canada (1750-2020)"
  )

# Optional challenge
# To add a subscript in your ggplot axes labels, you need to download the ggtext
# package. Ex: labs(x = "Subscript: CO<sub>2</sub>")


# Homework ----
# Question 1
vietnam <- annual_co_region %>% filter(Entity == "Vietnam")

# Graph the CO2 emission overtime in VN
ggplot(vietnam, aes(x = Year, y = CO2 / 1e8)) +
  geom_point() +
  geom_line() +
  labs(
    x = "Year",
    y = "Total CO2 Emission (100 million of tons in CO2)",
    title = "Total CO2 Emission in Vietnam 1750-2020"
  ) +
  theme_classic()

# Question 2
world_co2data <- annual_co_region %>% filter(Entity == "World")
world_temp_data <- climate_change %>% filter(Entity == "World")

# Graph change in world air temperatures
world_air_temp_graph <- ggplot(world_temp_data, aes(x=Day, y=temperature_anomaly)) +
  # Blue lower half (cooler)
  annotate("rect",
           xmin = -Inf, xmax = Inf, # add x and y boundaries 
           ymin = -Inf, ymax = 0,
           fill = "blue", alpha = 0.1) +
  
  # Red upper half (warmer)
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 0, ymax = Inf,
           fill = "red", alpha = 0.1) +
  geom_line() +
  geom_hline(yintercept = 0, color="red") + # add a horizontal line at 0
  labs(
    x = "Year",
    y = "Temperature Anomaly (Celsius)",
    title = "Rising World Temperature Anomaly (1880-2021)",
    caption = "Note: Temperature anomaly shows how much warmer or cooler a year is compared to the long-term average."
  ) +
  theme(
    plot.caption = element_text(size = 9, hjust = 0) # move the caption to be left-aligned with font 9
  )

# Graph change in CO2 emission for world
world_co2_emission_graph <- ggplot(world_co2data, aes(x=Year, y=CO2/1e9)) +
  geom_line() +
  labs(
    x = "Year",
    y = "CO2 Emission (billion of tons in CO2)",
    title = "World CO2 Emission (1750-2020)"
  ) +
  theme_classic()

# Question 3
# Read in data
carbon_footprint_data <- read.csv("carbon-footprint-travel-mode.csv")

# Remake the graph
ggplot(carbon_footprint_data,
       aes(x=Transport.emissions.per.kilometer.travelled, y=reorder(Entity, Transport.emissions.per.kilometer.travelled), # sort by carbon footprint
           fill=Entity)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = paste0(round(Transport.emissions.per.kilometer.travelled, 0), "g")), # customize the bar label
            hjust = -0.1) +  # labels slightly to the right of bars
  scale_fill_viridis_d(option = "plasma") + # discrete viridis palette (another library with color-blindness palatte)
  labs(
    x = NULL, # remove axes labels
    y = NULL,
    title = "Carbon footprint of travel per kilometer, 2022",
    subtitle = "The carbon footprint of travel is measured in grams of carbon dioxide-equivalents per passenger kilometer. This includes the
impact of increased warming from aviation emissions at altitude.",
    caption = "Data source: UK Government, Department for Energy Security and Net Zero (2022) – Learn more about this data

Note: Official conversion factors used in UK reporting. These factors will vary across countries depending on energy mix, transport
technologies, and occupancy of public transport. Data for aviation is based on economy class.

OurWorldinData.org/transport | CC BY"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # remove legend
    axis.text.x = element_blank(),   # remove x-axis numbers
    axis.ticks.x = element_blank(),   # remove x-axis ticks if you want
    plot.caption = element_text(size = 9, hjust = 0),
    panel.grid = element_blank() # remove gridlines
    )
