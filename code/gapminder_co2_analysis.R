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
  select(country, contient, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


# Dataset for analysis

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas")
  select(-year, -continent)

  