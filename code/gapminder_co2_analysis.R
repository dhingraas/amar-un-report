library(tidyverse)

#load in data
gapminder_data <- read_csv("data/gapminder_data.csv")


#Summarizing our data

summarize(gapminder_data, averageLifeExp = mean(lifeExp) )

gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>%
  summarize(averageLifeExp = mean(lifeExp))

#Filtering our data using filter()

gapminder_data %>%
  filter(year == 2007) %>%
  summarize(average = mean(lifeExp) )


#Find earliest year and then get the avg gdp for that year

gapminder_data %>%
  filter(year == min(year) ) %>%
  summarize(average_gdp = mean(gdpPercap))

# Grouping data

gapminder_data %>% 
    group_by(year) %>%
    summarize(average = mean(lifeExp) )

gapminder_data %>%
  group_by(continent) %>%
  summarize(average = mean(lifeExp) , min = min(lifeExp) )


#adding new columns with mutate()

gapminder_data %>%
    mutate(gdp = pop * gdpPercap, 
           popInMillions = pop / 1000000)

#Subset columns or change their order with select() 

gapminder_data %>%
  select(pop,year)



#Get rid of columns and order them
gapminder_data %>% 
  select(continent, country)


#Moving between long and wide data with pivot_wider() and pivot_longer

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


# Dataset for analysis

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)




#Data cleaning:  very often used


read_csv("data/co2-un-data.csv", skip = 1) #not the best

read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region", "country", "year" , "series",
                       "value", "footnotes", "source"))

read_csv("data/co2-un-data.csv", skip = 1) %>%
  rename(country = ...2)

read_csv("data/co2-un-data.csv", skip = 1) %>%
  rename_all(tolower)





co2_emissions_dirt <-  read_csv("data/co2-un-data.csv", skip = 2,
                         col_names = c("region", "country", "year" , "series",
                         "value", "footnotes", "source"))

#practice select, we only want country year series and value

co2_emissions_dirt %>%
  select(country,year,series, value) %>%
    mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions" ,
          "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions") ) %>%
    pivot_wider(names_from = series, values_from = value) %>%
    count(year)



co2_emissions <- co2_emissions_dirt %>%
  select(country,year,series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions" ,
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions") ) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)



#Joining data frames

inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions,
          by = "country")





co2_emissions <-  read_csv("data/co2-un-data.csv", skip = 2,
                                col_names = c("region", "country", "year" , "series",
                                              "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions" ,
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions") ) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States", 
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))


anti_join(gapminder_data_2007,co2_emissions, by = "country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))


anti_join(gapminder_data_2007, co2_emissions, by = "country")



gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop) )


inner_join(gapminder_data_2007, co2_emissions, by = "country")




gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country")



  