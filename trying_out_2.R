
# data processing
install.packages("R.utils")
library("R.utils")
fileurl <-  "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileurl, destfile =  "stormdata.csv.bz2")
bunzip2("stormdata.csv.bz2") # unzipping the stormdata
data <-  read.csv("stormdata.csv")

#Question 1: Across the United States, which types of events (as indicated in the \color{red}{\verb|EVTYPE|}EVTYPE variable)
# are most harmful with respect to population health?
# Here we shall explore and find out which event causes most injuries and fatalities
# At first lets select the key variables relevant in this analysis. These are;
# EVTYPE , FATALITIES , INJURIES, PROPDMG, CROPDMG, PROPDMGEXP, CROPDMGEXP .

library(dplyr)
data <- select(data, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG", "PROPDMGEXP", "CROPDMGEXP" ))
injuries <- aggregate(data$INJURIES, by = list(data$EVTYPE), FUN = sum) %>% 
  rename(event_type = "Group.1", total_injuries = "x") %>% arrange(desc(total_injuries))

head(injuries)
# we can see that TORNADO causes the most injuries (over 91,000), so it is the most harmful event type
# This is further shown in the figure below (only the first 5 events are shown)
library(ggplot2)
bar_plot_injuries <- ggplot(injuries[1:5,], aes(event_type, total_injuries)) +
  geom_bar(stat = "identity" , aes(fill = event_type)) +
  labs(x = "Type of Event", y = "Total injuries",
       title = "Total injuries by Event Type", 
       caption = "Figure 1: Only the first 5 devastating events are shown.
       Tornado shows highest number of injuries followed by TSTM Wind, 
       Flood,Excessive heat and lastly Lightning.") + 
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8.5,hjust = 0.5))

print(bar_plot_injuries)

# For the event type causing most fatalities;
library(dplyr)
fatalities <- aggregate(data$FATALITIES, by = list(data$EVTYPE), FUN = sum) %>% 
  rename(event_type = "Group.1", total_fatalities = "x") %>% arrange(desc(total_fatalities))
head(fatalities)
# we can see that still TORNADO causes the most fatalities(over 5 thousand dollars), which further supports that 
# supports that it is indeed the most harmful event type with respect to population
# health. The figure below shows this more clearly (only 5 event types are shown)

library(ggplot2)
bar_plot_fatalities <- ggplot(fatalities[1:5,], aes(event_type, total_fatalities)) +
  geom_bar(stat = "identity" , aes(fill = event_type)) + 
  labs(x = "Type of Event",
       y = "Total fatalities", 
       title = "Total fatalities by Event Type",
       caption = "Figure 2:Only the first 5 devastating event types are shown.
       Tornado shows the highest number of fatalities followed by Excessive heat,
       Flash flood, Heat, and lastly Lightning.") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8.5, hjust = 0.5))

# Question 2: Across the United States, which types of events have the greatest economic consequences?
# To address this question, we have to explore the property damange, crop damage
# and the total damage (property damage and crop damage combined)
# Let us begin with the property damage
# By looking at the PROPDMGEXP exponents;
unique(data$PROPDMGEXP)
# We see some lower case characters for example "m" instead of "M". We also see some symbols
# such as "+". We shall change lower case characters to upper case characters and also set 
# all symbols to zero character (since the class of the exponents is character)
data$PROPDMGEXP <- toupper(data$PROPDMGEXP)
data$PROPDMGEXP[data$PROPDMGEXP %in% c("", "+", "?", "-")] <-  "0"

# Since PROPDMGEXP stands for the power of 10, we need to convert
# 'B' (for billions) to 9, 'M'( for millions) to 6, 'K' ( for thousands) to 3 
# and 'H'(for hundreds) to 2.

data$PROPDMGEXP[data$PROPDMGEXP == "B"] <- "9"
data$PROPDMGEXP[data$PROPDMGEXP == "M"] <- "6"
data$PROPDMGEXP[data$PROPDMGEXP == "K"] <- "3"
data$PROPDMGEXP[data$PROPDMGEXP == "H"] <- "2"

# We shall now convert the character class of PROPDMGEXP to numeric class and 
# calculating  the true damage by multiplying the damage by the corresponding exponent.
library(dplyr)
data$PROPDMGEXP <- 10^(as.numeric(data$PROPDMGEXP))
damage <- mutate(data, true.property.damage = PROPDMG*PROPDMGEXP) 
damage1 <- aggregate(damage$true.property.damage, by = list(damage$EVTYPE), FUN = sum) %>% 
  rename(Event.type = "Group.1", total.property.damage = "x") %>% 
  arrange(desc(total.property.damage))
  head(damage1)
data.frame(Event.type = damage1$Event.type[which.max(damage1$total.property.damage)],
Amount.dollars = damage1$total.property.damage[which.max(damage1$total.property.damage)])

# we see that Flood shows the greatest economic loss in terms of property damage with over 144 billion dollars!
# Likewise, let us find out which event type shows the biggest economic loss in case of crops.
# Let us just continue with the damage dataset
unique(damage$CROPDMGEXP)
# converting lower case characters to uppercase characters and also setting symbols to zero
damage$CROPDMGEXP <- toupper(damage$CROPDMGEXP)
damage$CROPDMGEXP[damage$CROPDMGEXP %in% c("", "+", "?", "-")] <-  "0"


# Since CROPDMGEXP stands for the power of 10, we need to convert
# 'B' (for billions) to 9, 'M'( for millions) to 6, 'K' ( for thousands) to 3 
# and 'H'(for hundreds) to 2.

damage$CROPDMGEXP[damage$CROPDMGEXP == "B"] <- "9"
damage$CROPDMGEXP[damage$CROPDMGEXP == "M"] <- "6"
damage$CROPDMGEXP[damage$CROPDMGEXP == "K"] <- "3"
damage$CROPDMGEXP[damage$CROPDMGEXP == "H"] <- "2"


# converting the character class of CROPDMGEXP variable to numeric class and 
# calculating  the true damage by multiplying the damage by the corresponding exponent.
library(dplyr)
damage$CROPDMGEXP <- 10^(as.numeric(damage$CROPDMGEXP))
damagex <- mutate(damage, true.crop.damage = CROPDMG*CROPDMGEXP)
damage2 <- aggregate(damagex$true.crop.damage, by = list(damagex$EVTYPE), FUN = sum) %>% 
  rename(Event.type = "Group.1", total.crop.damage = "x") %>% 
  arrange(desc(total.crop.damage))
head(damage2)
data.frame(Event.type= damage2$Event.type[which.max(damage2$total.crop.damage)],
           Amount.dollars = damage2$total.crop.damage[which.max(damage2$total.crop.damage)])

# We can see that Drought causes the most damage with an economic loss of over 13 billion dollars!



# Finding the total damage by getting the sum of damage to property and that of crops
Total_damage <- merge(damage1,damage2) %>% 
  mutate(total.damage = total.property.damage + total.crop.damage) %>% 
  arrange(desc(total.damage)) %>%
  select(c("Event.type", "total.damage"))
head(Total_damage)
data.frame(Event.type= Total_damage$Event.type[which.max(Total_damage$total.damage)],
           Amount.dollars = Total_damage$total.damage[which.max(Total_damage$total.damage)])
# We can see that Flood causes the most damage with a huge economic loss of over 150 billion dollars!

# This can be shown further in the figure below;
library(ggplot2)
bar_plot_total_damage <- ggplot(Total_damage[1:5,], aes(Event.type, total.damage))+
  geom_bar(aes(fill = Event.type), stat = "identity") + 
  labs(x = "Type of event", 
       y = "Total Damage", 
       title = "Total damage by Event type",
       caption = "Figure 3 : Only the first 5 devastating event types are shown.
       Flood shows the highest damage followed by Hurricane/Typhoon, Tornado, Storm surge,
       and lastly Hail." )+
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(size = 8.5, hjust = 0.5))

print(bar_plot_total_damage)



# captions
# Figure 1: Only the first 5 devastating events are shown.Tornado shows highest 
# number of injuries followed by TSTM Wind, Flood,Excessive heat and lastly Lightning.

# Figure 2:Only the first 5 devastating event types are shown.Tornado shows the highest
# number of fatalities followed by Excessive heat, Flash flood, Heat, and lastly Lightning.

# Figure 3 : Only the first 5 devastating event types are shown.Flood shows the highest
# damage followed by storm surge, Hurricane/Typhoon and lastly River flood.