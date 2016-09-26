library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)
library(trendct)


library(extrafont)
library(ggalt)
library(scales)
library(gridExtra)
library(grid)
source("keys.R")

mj <- read.csv("data/15-218_Marijuana_Arrests_by_Agency_2011-2013_NIBRS.csv", stringsAsFactors=F)

colnames(mj) <- c("date", "description", "gender", "race", "ethnicity", "age")

mj2 <- filter(mj,
              date!="" &
              !grepl("Unit of", date) &
              !grepl("NSS ", date) &
              !grepl("Report ", date) &
              !grepl("Arrests ", date) &
              !grepl("IncidentDate", date)
                )

mj2$department <- "temp"

department_name <- "temp"

for (i in 1:nrow(mj2)) {
  #print(i)
  department_name <- ifelse(grepl("/", mj2$date[i]), department_name, mj2$date[i])
  mj2$department[i] <- department_name
  #print( mj2$department[i] )
}

mj2 <- mj2 %>%
  filter(gender=="M" | gender=="F")

mj2$date <- mdy(mj2$date)
mj2$year <- year(mj2$date)
mj2$month <- month(mj2$date)


## Men and women

mj_sex <- mj2 %>%
  group_by(gender) %>%
  summarise(citations=n())

# departments that arrested the most

mj_most <- mj2 %>%
  group_by(department) %>%
  summarise(arrests=n()) %>%
  arrange(-arrests)

head(mj_most)


# arrests over time by department

mj_arrests_years <- mj2 %>%
  group_by(department, year) %>%
  summarise(arrests=n())

mj_arrests_years$town <- gsub(" Police Department", "", mj_arrests_years$department)
mj_arrests_years$town <- gsub(" Police Dept.", "", mj_arrests_years$town)

library(ctnamecleaner)
mj_arrests_years <- ctpopulator(town, mj_arrests_years)
mj_arrests_years2 <- subset(mj_arrests_years, !is.na(pop2013))
mj_arrests_years2$per_capita <- mj_arrests_years2$arrests/mj_arrests_years2$pop2013*10000
## chart

gg <- ggplot(mj_arrests_years, aes(x=year, y=arrests)) 
gg <- gg + geom_bar(stat="identity") 
gg <- gg + facet_wrap(~department, ncol = 3)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme_bw(base_family="Lato Regular")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
##gg

#ggsave(gg, file="citations-raw.svg", width=12, height=40)
ggsave(gg, file="citations-raw.png", width=6, height=20, units="in", dpi=150)


singleString <- paste(readLines("citations-raw.svg"), collapse=" ")
singleString <- gsub("]]>", "]]>@import url(http://fonts.googleapis.com/css?family=Lato);", singleString )
singleString <- gsub(" Regular", ";", singleString )

write(singleString, "citations-raw.svg")




ggplot(mj_arrests_years2, aes(x=year, y=per_capita)) + geom_bar(stat="identity") + facet_wrap(~department, ncol = 3)

gg <- ggplot(mj_arrests_years2, aes(x=year, y=per_capita)) 
gg <- gg + geom_bar(stat="identity") 
gg <- gg + facet_wrap(~department, ncol = 3)
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + theme_bw(base_family="Lato Regular")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
##gg

#ggsave(gg, file="citations-adjusted.svg", width=12, height=40)
ggsave(gg, file="citations-adjusted.png", width=6, height=20, units="in", dpi=150)

singleString <- paste(readLines("citations-raw.svg"), collapse=" ")
singleString <- gsub("]]>", "]]>@import url(http://fonts.googleapis.com/css?family=Lato);", singleString )
singleString <- gsub(" Regular", ";", singleString )

write(singleString, "citations-raw.svg")




mj_arrests_years <- mj2 %>%
  group_by(department, year) %>%
  summarise(arrests=n()) %>%
  spread(year, arrests) %>%
  mutate(per_change=round((`2013` - `2011`) / `2011` *100, 2)) %>%
  arrange(-per_change)

head(mj_arrests_years)



# arrests by race by department
mj2$race_ethnicity <- ifelse(mj2$ethnicity=="H", "Hispanic", mj2$race)
mj2$race_ethnicity <- ifelse(mj2$race_ethnicity=="A", "Asian", mj2$race_ethnicity)
mj2$race_ethnicity <- ifelse(mj2$race_ethnicity=="B", "Black", mj2$race_ethnicity)
mj2$race_ethnicity <- ifelse(mj2$race_ethnicity=="W", "White", mj2$race_ethnicity)

mj2$race_ethnicity <- ifelse(mj2$race_ethnicity=="I", "Indian", mj2$race_ethnicity)
mj2$race_ethnicity <- ifelse(mj2$race_ethnicity=="U", "Unknown", mj2$race_ethnicity)


mj_arrests_race <- mj2 %>%
  group_by(department, race_ethnicity) %>%
  summarise(arrests=n())

## chart
ggplot(mj_arrests_race, aes(x=race_ethnicity, y=arrests)) + geom_bar(stat="identity") + coord_flip() + facet_wrap(~department, ncol = 4, scales = "free_x")

## bring in census data

# If you do not yet have the censusapi package installed, uncomment the lines below and run them.

#install.packages("devtools")
#devtools::install_github("hrecht/censusapi")
library("censusapi")

# B02001_001E - Total
# B02001_002E - White
# B02001_003E - Black
# B02001_004E - Indian
# B02001_005E - Asian
# B03001_003E - Hispanic

race_towns <- getCensus(name="acs5",
                         vintage=2014,
                         key=census_key,
                         vars=c("NAME", "B02001_001E", "B02001_002E", "B02001_003E",
                                "B02001_004E", "B02001_005E", "B03001_003E"),
                         region="county subdivision:*", regionin="state:09")

colnames(race_towns) <- c("town", "state", "county", "countysub", "total_pop", "White", "Black", "Indian", "Asian", "Hispanic")
race_towns <- race_towns[c("town", "total_pop", "White", "Black", "Indian", "Asian", "Hispanic")]
race_towns <- subset(race_towns, !grepl("County subdivisions", town))
race_towns$town <- gsub(" town.*", "", race_towns$town)

race_towns_long <- race_towns %>%
  gather("race_ethnicity", "population", 3:7) %>%
  mutate(percent_population=round(population/total_pop*100,2))


#percent of tickets by race compared to percent of population by race


mj_arrests_race_spread <- mj_arrests_race %>%
  group_by(department) %>%
  mutate(total=sum(arrests, na.rm=T), percent=round(arrests/total*100,2))

mj_arrests_race_spread$town <- gsub(" Police Department", "", mj_arrests_race_spread$department)
mj_arrests_race_spread$town <- gsub(" Police Dept.", "", mj_arrests_race_spread$town)



mj_arrests_race_spread <- left_join(mj_arrests_race_spread, race_towns_long)

mj_arrests_filtered <- subset(mj_arrests_race_spread, !is.na(total_pop))
mj_arrests_filtered <- filter(mj_arrests_filtered, race_ethnicity!="Indian")

mj_filtered <- mj_arrests_filtered[c ("percent", "percent_population")]

gg <- ggplot(mj_arrests_filtered, aes(percent, percent_population)) 
gg <- gg + geom_abline(intercept = 1, color="grey65")
gg <- gg + geom_point(data = mj_filtered, color = "grey85") 
gg <- gg + geom_point(aes(color=race_ethnicity)) 
gg <- gg + facet_wrap(~race_ethnicity, ncol=4)
gg <- gg + labs(y="Percent population", x="Percent cited", 
                title="Marijuana citations by race compared to town's population", 
                subtitle="Minorities tend to be cited disproportionately to the proportion of the population of the towns they live in.\nBased on citations between 2011 and 2014.", 
                caption="SOURCE: National Incident-Based Reporting System, U.S. Census \nAndrew Ba Tran/TrendCT.org")
gg <- gg + theme_bw(base_family="Lato Regular")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg <- gg + theme(plot.title=element_text(face="bold", family="Lato Regular", size=22))
gg <- gg + theme(plot.caption=element_text(face="bold", family="Lato Regular", size=9, color="gray", margin=margin(t=10, r=80)))
gg <- gg + theme(legend.position="none")
gg



ggsave(gg, file="citations-race.svg", width=12, height=6)
ggsave(gg, file="citations-race.png", width=9, height=4.5, units="in", dpi=150)


singleString <- paste(readLines("citations-race.svg"), collapse=" ")
singleString <- gsub("]]>", "]]>@import url(http://fonts.googleapis.com/css?family=Lato);", singleString )
singleString <- gsub(" Regular", ";", singleString )

write(singleString, "citations-race.svg")



# gotta adjust for population...

mj_most$town <- gsub(" Police Department", "", mj_most$department)
mj_most$town <- gsub(" Police Dept.", "", mj_most$town)

library(ctnamecleaner)
mj_most2 <- ctpopulator(town, mj_most)

non_mj_most <- subset(mj_most2, is.na(pop2013))
write.csv(non_mj_most, "data/leftovers_total.csv")

mj_most2_map <- subset(mj_most2, !is.na(pop2013))
mj_most2_map$per_capita <- (mj_most2_map$arrests/mj_most2_map$pop2013)*10000

mj_most2_map <- mj_most2_map[c("town", "per_capita", "arrests")]
colnames(mj_most2_map) <- c("Town", "Per capita arrets", "Total arrests")

mj_most2_map$Town <- str_to_title(mj_most2_map$Town)
write.csv(mj_most2_map, "data/total_towns.csv")

trendmap(mj_most2_map, headline="Marijuana citations by town", subhead="Between 2011 and 2014. Per capita is per 10,000 residents.",
         src=" National Incident-Based Reporting System", byline="TrendCT.org", url_append="date", shape="towns", color="yellow-red",
         keys = ftp_login)

## Age

mj2$age <- as.numeric(mj2$age)

ggplot(mj2, aes(mj2$age)) + geom_histogram(binwidth=1, aes(fill = ..count..)) + ggtitle("Marijuana citations by age in Connecticut")

age_list <- data.frame(seq(10,78, by=1))
colnames(age_list) <- "age"

mj_age <- mj2 %>%
  group_by(age) %>%
  summarise(citations=n())

age_list <- left_join(age_list, mj_age)
age_list[is.na(age_list)] <- 0

trendchart(age_list, headline = "Age distribution of those cited for marijuana ", subhead = "In Connecticut between 2011 and 2014.", src = "National Incident-Based Reporting System",
           byline = "TrendCT.org", type = "column", xTitle = "", yTitle = "",
           xSuffix = "", ySuffix = "", xPrefix = "", yPrefix = "", option = "",
           keys = ftp_login)

