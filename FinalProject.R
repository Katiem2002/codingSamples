
pacman::p_load(tidyverse, rmarkdown, htmlwidgets, htmltools, maps,
               leaflet, kableExtra, tibble, dplyr, gridExtra, viridis)

countries <- read.csv("/Users/katiemoses/Documents/CSCI444/Final Project/data/countries.csv")

covid <- read.csv("/Users/katiemoses/Documents/CSCI444/Final Project/data/owid-covid-data.csv")

#citation for covid dataset (open source)
#' @article{owidcoronavirus,
#'   author = {Edouard Mathieu and Hannah Ritchie and Lucas Rodés-Guirao and Cameron Appel and Charlie Giattino and Joe Hasell and Bobbie Macdonald and Saloni Dattani and Diana Beltekian and Esteban Ortiz-Ospina and Max Roser},
#'   title = {Coronavirus Pandemic (COVID-19)},
#'   journal = {Our World in Data},
#'   year = {2020},
#'   note = {https://ourworldindata.org/coronavirus}
#' }



#QUESTION 1
#Compare the 2019 health related expenditures across all world nations (with obtainable data) to the average new cases per day in June 2020? Determine the 5 countries with the highest relative health expenditures in 2019. Determine the 5 countries with the lowest daily new Covid19 cases in June 2020. Is there overlap between high health expenditures and reduced new Covid19 cases? 

#get variable names for countries df 
variableNames <- colnames(countries)
view(variableNames)

#df with countries (with available data) and their respective health expenditures (as of 2019) **expressed as a percent of each nation's GDP**
health <- countries %>%
  select("Country", "People.and.Society..Current.health.expenditure") %>%
  mutate(`Health Expenditure (as percentage of GDP)` = as.numeric(sub("%.*", "", `People.and.Society..Current.health.expenditure`))) %>%
  filter(!is.na(`Health Expenditure (as percentage of GDP)`)) %>%
  select(-`People.and.Society..Current.health.expenditure`)
view(health)

#covid df -- average new cases per day in June 2020 by country
#LOOK AT NEW_CASES VS NEW_CASES_SMOOTHED
#use new_cases_per_million to normalize for differing populations in countries 
covidJune <- covid %>%
  select("location", "date", "new_cases_per_million") %>%
  mutate(date = as.Date(date)) %>%
  filter(format(date, "%Y-%m") == "2020-06") %>%
  group_by(location) %>%
  mutate(`Avg New Cases (Per Million People) Per Day June 2020` = round(mean(new_cases_per_million),1)) %>%
  select(-new_cases_per_million, -date) %>%
  distinct(location, .keep_all = TRUE) %>%
  rename(Country = location)

view(covidJune)

#inner join covidJune and health so that only countries included in both dfs are included 
#health included 190 countries and combined includes 180 countries 
combined <- inner_join(covidJune, health, by = "Country")
view(combined)

#top 5 countries for health expenditure 
#TRY TO DO THIS USING SLICE_MAX
top5health <- combined %>%
  arrange(desc(`Health Expenditure (as percentage of GDP)`)) %>%
  head(5)
view(top5health)

#top 5 countries for lowest average daily new covid cases, June 2020
top5cases <- combined %>%
  arrange(`Avg New Cases (Per Million People) Per Day June 2020`) %>%
  head(5)
view(top5cases)

#create tables for R Markdown 
#table for max health expenditure 

top5health %>%
  kable(align = "c", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(0, color = "white", background = "#18488C") %>%
  row_spec(1:5, color = "#18488C")

#table for min covid cases June 2020

top5cases %>%
  kable(align = "c", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(0, color = "white", background = "#18488C") %>%
  row_spec(1:5, color = "#18488C")



view(covid)




#QUESTION 2
#Examine Covid deaths in all world nations between June 2020 and June 2023. Examine population density, GDP per capita, and 2019 health related expenditures in all world nations. Which of these factors is most closely related to increased Covid deaths? 

#get covid deaths avg 2020-2023 by country
covidDeaths <- covid %>%
  select(location, date, new_deaths_per_million, continent) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("2020-01-03") & date <= as.Date("2023-10-25")) %>%
  group_by(location) %>%
  filter(!is.na(new_deaths_per_million)) %>%
  mutate(avgDeaths2020to2023 = round(mean(new_deaths_per_million),3)) %>%
  select(-date, -new_deaths_per_million) %>%
  distinct(location, .keep_all = TRUE) %>%
  rename(Country = "location")
view(covidDeaths)

#make df with population density, GDP per capita and health expenditure info 
#health expenditure data mostly considers the year 2019, GDP per capita considers most recent estimate (furthest back i saw was 2003 but most were 2021 estimates) 
countryData <- countries %>%
  select("Country", "People.and.Society..Current.health.expenditure", "Economy..Real.GDP.per.capita", "People.and.Society..Median.age...total") %>%
  mutate(`Health Expenditure` = as.numeric(sub("%.*", "", `People.and.Society..Current.health.expenditure`))) %>%
  select(-People.and.Society..Current.health.expenditure) %>%
  mutate(`GDP per Capita` = str_replace_all(`Economy..Real.GDP.per.capita`, "\\s.*", "")) %>%
  mutate(`GDP per Capita` = str_replace_all(`GDP per Capita`, "[$,]", "")) %>%
  mutate(`GDP per Capita` = as.numeric(`GDP per Capita`)) %>%
  select(-Economy..Real.GDP.per.capita) %>%
  filter(!is.na(`GDP per Capita`)) %>%
  filter(!is.na(`Health Expenditure`)) %>%
  rename(medianAge = "People.and.Society..Median.age...total") %>%
  mutate(medianAge = str_replace_all(`medianAge`, "\\s.*", "")) %>%
  mutate(medianAge = as.numeric(medianAge))
view(countryData)
view(countries)


#join covid df population density column to countryData 
#population density with all data points
popDen <- covid %>%
  select(location, population_density) %>%
  distinct(population_density, .keep_all = TRUE) %>%
  filter(!is.na(population_density)) %>%
  rename(Country = "location")
view(popDen) 

totalCountryData <- inner_join(countryData, popDen, by = "Country")
view(totalCountryData)

#join totalCountryData and covidDeaths 
age_brackets <- seq(0, 100, 10)

scatterplotData <- inner_join(covidDeaths, totalCountryData, by = "Country")

scatterplotData <- scatterplotData %>%
  filter(Country != "World") %>%
  mutate(age_group = cut(medianAge, breaks = age_brackets, labels = paste0(age_brackets[-length(age_brackets)], "-", age_brackets[-1])))
view(scatterplotData)

#create the 3 scatterplots 
#deaths vs health expenditure 
plot1 <- scatterplotData %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `Health Expenditure`, color = continent)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `Health Expenditure`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Health Expenditure (as % of GDP)", title = "COVID-19 Deaths Vs. Health Expenditure") 

plot2 <- scatterplotData %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`, color = continent)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `GDP per Capita`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "GDP Per Capita", title = "COVID-19 Deaths Vs. GDP Per Capita") 

plot3 <- scatterplotData %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `population_density`, color = continent)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `population_density`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Population Density", title = "COVID-19 Deaths Vs. Population Density") 

print(plot1)
print(plot2)
print(plot3)
grid.arrange(plot1, plot2, plot3, ncol=1)

#population density without outliers to better visualize trend 
plot4 <- scatterplotData2 %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `population_density`, color = continent)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `population_density`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Population Density", title = "COVID-19 Deaths Vs. Population Density") 
print(plot4)

#what were the two density outliers?
popDenOutliers <- covid %>%
  select(location, population_density) %>%
  distinct(population_density, .keep_all = TRUE) %>%
  filter(!is.na(population_density)) %>%
  rename(Country = "location") %>%
  filter(population_density>=7000) #shows the three outliers
view(popDenOutliers) 
#population density outliers are Hong Kong, Macao, Monaco, and Singapore


#scatterplots with with top and bottom 5 for each category listed above
#Health Expenditure
top5HEX <-  scatterplotData %>%
  select(Country, `Health Expenditure`, `avgDeaths2020to2023`) %>%
  arrange(desc(`Health Expenditure`)) %>%
  head(5)

low5HEX <- scatterplotData %>%
  select(Country, `Health Expenditure`, `avgDeaths2020to2023`) %>%
  arrange(`Health Expenditure`) %>%
  head(5)

HEX <- rbind(top5HEX, low5HEX)

plotHEX <- HEX %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `Health Expenditure`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `Health Expenditure`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `Health Expenditure`, label = Country), size = 2, hjust = 0.5, vjust = ifelse(HEX$Country == "Benin", -1, 1.5)) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Health Expenditure", title = "COVID-19 Deaths Vs. Health Expenditure") 
print(plotHEX)

#GDP Per Capita
top5GDP <-  scatterplotData %>%
  select(Country, `GDP per Capita`, `avgDeaths2020to2023`) %>%
  arrange(desc(`GDP per Capita`)) %>%
  head(5)

low5GDP <-  scatterplotData %>%
  select(Country, `GDP per Capita`, `avgDeaths2020to2023`) %>%
  arrange(`GDP per Capita`) %>%
  head(5)

GDP <- rbind(top5GDP, low5GDP)

plotGDP <- GDP %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `GDP per Capita`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "GDP per Capita", title = "COVID-19 Deaths Vs. GDP per Capita") 
print(plotGDP)

#zoom in of low5GDP
plotGDP2 <- GDP %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `GDP per Capita`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "GDP per Capita", title = "COVID-19 Deaths Vs. GDP per Capita") + 
  coord_cartesian(xlim = c(0, 0.1), ylim = c(0, 2000)) #crop scatterplot for low5GDP only
print(plotGDP2)

#Population Density
top5DEN <-  scatterplotData %>%
  select(Country, `population_density`, `avgDeaths2020to2023`) %>%
  arrange(desc(`population_density`)) %>%
  head(5)

low5DEN <-  scatterplotData %>%
  select(Country, `population_density`, `avgDeaths2020to2023`) %>%
  arrange(`population_density`) %>%
  head(5)

DEN <- rbind(top5DEN, low5DEN)

plotDEN <- DEN %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `population_density`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `population_density`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `population_density`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Population Density", title = "COVID-19 Deaths Vs. Population Density") 
print(plotDEN)

#taking out outliers
top5DEN2 <-  scatterplotData2 %>%
  select(Country, `population_density`, `avgDeaths2020to2023`) %>%
  arrange(desc(`population_density`)) %>%
  head(5)

low5DEN2 <-  scatterplotData2 %>%
  select(Country, `population_density`, `avgDeaths2020to2023`) %>%
  arrange(`population_density`) %>%
  head(5)

DEN2 <- rbind(top5DEN2, low5DEN2)

plotDEN2 <- DEN2 %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `population_density`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `population_density`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `population_density`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Population Density", title = "COVID-19 Deaths Vs. Population Density") 
print(plotDEN2)


#plot population density without outliers 
#popualtion density with outliers removed (easier analysis of viz) 
popDen2 <- covid %>%
  select(location, population_density) %>%
  distinct(population_density, .keep_all = TRUE) %>%
  filter(!is.na(population_density)) %>%
  rename(Country = "location") %>%
  filter(population_density<7000) #removes four outliers, trend line is still essentially the same

totalCountryData2 <- inner_join(countryData, popDen2, by = "Country")

scatterplotData2 <- inner_join(covidDeaths, totalCountryData2, by = "Country")

scatterplotData2 <- scatterplotData2 %>%
  filter(Country != "World")

#get r values
#covid deaths vs health expenditure
r_value <- cor(scatterplotData$avgDeaths2020to2023, scatterplotData$`Health Expenditure`)
print(r_value)

#covid deaths vs GDP per Capita 
r_value2 <- cor(scatterplotData$avgDeaths2020to2023, scatterplotData$`GDP per Capita`)
print(r_value2)

#covid deaths vs population density
r_value3 <- cor(scatterplotData$avgDeaths2020to2023, scatterplotData$`population_density`)
print(r_value3)

#covid deaths vs population density (minus outliers)
r_value4 <- cor(scatterplotData2$avgDeaths2020to2023, scatterplotData2$`population_density`)
print(r_value4)

#covid deaths vs age structure (based on countries median age)
r_value5 <- cor(scatterplotData$avgDeaths2020to2023, scatterplotData$`medianAge`)
print(r_value5)

#scatterplot with COVID deaths vs GDP per Capita, color by age structure 
plot5 <- scatterplotData %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `GDP per Capita`, color = age_group)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `GDP per Capita`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  scale_color_brewer(palette = "Set2", name = "Median Age") +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "GDP Per Capita", title = "COVID-19 Deaths Vs. GDP Per Capita Colored by Age Structure") 
print(plot5)

#top 5 and low 5 for age
top5AGE <-  scatterplotData %>%
  select(Country, `medianAge`, `avgDeaths2020to2023`) %>%
  arrange(desc(`medianAge`)) %>%
  head(5)

low5AGE <-  scatterplotData %>%
  select(Country, `medianAge`, `avgDeaths2020to2023`) %>%
  arrange(`medianAge`) %>%
  head(5)

AGE <- rbind(top5AGE, low5AGE)

plotAGE <- AGE %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `medianAge`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `medianAge`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `medianAge`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Median Age", title = "COVID-19 Deaths Vs. Median Age") 
print(plotAGE)

#zoom in on 5 lowest median ages 
plotAGE2 <- AGE %>%
  ggplot() + 
  geom_point(mapping = aes(x=`avgDeaths2020to2023`, y = `medianAge`)) +
  geom_smooth(mapping = aes(x = `avgDeaths2020to2023`, y = `medianAge`), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.5) +
  geom_text(aes(x=`avgDeaths2020to2023`, y = `medianAge`, label = Country), size = 3, hjust = 0.5, vjust = 1.75) +
  labs(x="Average Deaths Per Day (01/03/2020 to 10/25/2023)", y = "Median Age", title = "COVID-19 Deaths Vs. Median Age") +
  coord_cartesian(xlim = c(0, 0.07), ylim = c(0, 20)) #crop scatterplot for low5GDP only
print(plotAGE2)






#QUESTION 3
#Determine for each world nation the day in which the most vaccinations were administered. Assess covid rates before and after this day for each nation. What is the trend of new covid cases before and after the day in which the most vaccines were administered? 

#create df to find cases average before max vaccination date
#biden said pandmeic was over on Sept 19 2022, so limit dates to 01/03/2020 until 09/18/2022
cases <- covid %>%
  select(location, date, new_cases_per_million) %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2022-09-18")) %>%
  filter(!is.na(new_cases_per_million)) %>%
  group_by(location)
view(cases)

vaccines <- covid %>%
  select(location, date, new_vaccinations_smoothed_per_million) %>%
  mutate(date = as.Date(date)) %>%
  filter(date < as.Date("2022-09-18")) %>%
  filter(!is.na(new_vaccinations_smoothed_per_million)) %>%
  group_by(location) %>%
  slice_max(new_vaccinations_smoothed_per_million, n=1) %>%
  rename(maxVacc = "new_vaccinations_smoothed_per_million") %>%
  distinct(location, .keep_all = TRUE)
view(vaccines)

#possible issue with data?? says Wallis and Futuna max vaccination day administered 37254 vaccines on 04/04/2022. Looked up total population of W and F and population is ~15,000. 
#maybe create df with coutries who report more vaccines administered on max vaccination day than total population of country???? 
WandF <- covid %>%
  select(location, date, new_vaccinations_smoothed_per_million) %>%
  filter(location == "Wallis and Futuna") %>%
  filter(!is.na(new_vaccinations_smoothed_per_million))
view(WandF)

#look at covid cases for 6 months before and 6 months after covid vaccination max day (because vaccines work for approx. 6 months) 

vaccineTrendsBefore <- cases %>%
  left_join(vaccines, by = "location") %>%
  group_by(location) %>%
  filter(date.x < date.y) %>%
  mutate(avgCasesBefore = round(mean(new_cases_per_million),3)) %>%
  distinct(location, .keep_all = TRUE)

vaccineTrendsAfter <- cases %>%
  left_join(vaccines, by = "location") %>%
  group_by(location) %>%
  filter(date.x >= date.y) %>%
  mutate(avgCasesAfter = round(mean(new_cases_per_million),3)) %>%
  distinct(location, .keep_all = TRUE)

view(vaccineTrendsBefore)
view(vaccineTrendsAfter)

#huge outliers, take these out
comparison <- vaccineTrendsBefore %>%
  left_join(vaccineTrendsAfter, by = "location") %>%
  select(location, date.y.y, maxVacc.y, avgCasesBefore, avgCasesAfter) %>%
  rename(`Date of Maximum Vaccinations` = "date.y.y") %>%
  rename(`Number of Vaccinations Administered on Date of Maximum Vaccinations` = "maxVacc.y")%>%
  filter(avgCasesBefore != 0) %>%
  mutate(`Percent Growth` = round(((avgCasesAfter - avgCasesBefore)/avgCasesBefore)*100,3)) %>%
  mutate(sign = case_when(
    `Percent Growth` > 0 ~ "positive", 
    `Percent Growth` < 0 ~ "negative", 
    `Percent Growth` == 0 ~ "0"
  )) %>%
  mutate(`Percent Growth` = abs(`Percent Growth`)) %>%
  mutate(logPercentGrowth = log(`Percent Growth`, 20))

logGrowthPositive <- comparison %>%
  filter(sign == "positive") 

logGrowthNegative <- comparison %>%
  filter(sign == "negative")


#cloropleth, map percent growth of covid in relation to max vaccination date (color negative percent **less covid** as green and positive percent **more covid** as red)
worldMap <- map_data("world") %>%
  rename(location = "region") %>%
  mutate(location = ifelse(location == "USA", "United States", location)) %>%
  mutate(location = ifelse(location == "Democratic Republic of the Congo", "Democratic Republic of Congo", location))

#positive
worldComparisonPositive <- worldMap %>%
  left_join(logGrowthPositive, by = "location") %>%
  select(-subregion) %>%
  filter(!is.na(`Percent Growth`))

#negative 
worldComparisonNegative <- worldMap %>%
  left_join(logGrowthNegative, by = "location") %>%
  select(-subregion) %>%
  filter(!is.na(`Percent Growth`)) %>%
  mutate(logPercentGrowth = logPercentGrowth*-1)


cloropleth <- 
  ggplot() +
  geom_polygon(data = worldComparisonPositive, aes(x = long, y = lat, group = group, fill = logPercentGrowth)) +
  geom_polygon(data = worldComparisonNegative, aes(x = long, y = lat, group = group, fill = logPercentGrowth)) +
  coord_equal() +
  labs(title = "Covid Growth Rates", subtitle = "Comparison of New COVID-19 Cases Before and After Maximum Vaccination Day") + 
  scale_fill_gradientn(
    name = "COVID-19 Growth Rate (%)",
    colours = c("blue", "white", "red"), 
    values = scales::rescale(c(-5, 0, 5)), 
    limits = c(-5, 5),  
    breaks = seq(-5, 5, by = 1), 
    labels = seq(-5, 5, by = 1), 
    guide = "legend" 
  ) +
  guides(fill = guide_colorbar(barwidth = 2, barheight = 5))  
print(cloropleth)

#chart showing when countries had their day of maximum vaccinations
maxVaccDays <- comparison %>%
  select(location, `Date of Maximum Vaccinations`)

maxVaccDays %>%
  kable(align = "c", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(0, color = "white", background = "#18488C") %>%
  row_spec(1:219, color = "#18488C")
  
view(maxVaccDays)

#max vax days for selected countries
maxVaccDaysSelected <- maxVaccDays %>%
  filter(location %in% c("United States", "Brazil", "Germany", "China", "Australia", "Zimbabwe", "Egypt", "Norway", "Japan")) %>%
  arrange(`Date of Maximum Vaccinations`)

maxVaccDaysSelected %>%
  kable(align = "c", escape = FALSE) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(0, color = "white", background = "#18488C") %>%
  row_spec(1:9, color = "#18488C")
