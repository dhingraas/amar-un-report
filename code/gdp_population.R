library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv")

name <- "ben"
name
age <-27

Sys.Date()    #outputs the current date
getwd()  # outputs the working directory



#Plotting!
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  geom_point() +
  labs(title ="Do people in weathly countries live longer?")+
  aes(color = continent) +
 # scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000)+
  labs(size = "Population (in millions")

  #save time by using aes and lab
ggplot(data = gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  labs(x = "GDP Per Capita", y = "Life Expectancy",
       title = "Do people in wealthy countries live longer?", size = "Population (in millions)")






# Plotting for data exploration
  
gapminder_data <- read_csv("gapminder_data.csv")
  
ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent) +
  geom_point()   # not the best fit
  
  str(gapminder_data)  #structure of the data
  
  
  ggplot(data = gapminder_data) +
    aes(x = year, y = lifeExp, color = continent, group = country) +
    geom_line()  #very bad fit without the group = country
  
  ggplot(data = gapminder_data) +
    aes(x = continent, y = lifeExp) +
    geom_boxplot()  
  
  ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
    geom_violin( aes(fill = continent), color = "gold") +
    geom_jitter(alpha = 0.5)  #change transparency

  
  ggplot(gapminder_1997) +
    aes(x = lifeExp) + 
    geom_histogram(bins = 20)
  
  
  #use the cheat sheet under the help bar which is under and to thr right of addins
  
  ggplot(gapminder_1997) +
    aes(x = lifeExp) + 
    geom_density()   #bell curve
  
  
  # ggplot2 Themes
  
  
  ggplot(gapminder_1997) +
    aes(x = lifeExp) + 
    geom_histogram() +
   # scale_x_reverse()+     # reversed the x-axis
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) #angles the data numbers on an angle and down and over
    theme_minimal()   #change the plot background/theme
  

    
    #Facet a plot
    
    ggplot(data = gapminder_1997) +
      aes(x = gdpPercap, y = lifeExp) +
      geom_point() +
      facet_wrap(vars(continent)) #creates mini-plots based off the continent, allows better visualization
      
    
    ggplot(data = gapminder_1997) +
      aes(x = gdpPercap, y = lifeExp) +
      geom_point() +
      facet_grid(rows = vars(continent))  #separates y-axis but they all have the same x-axis
    
    
    
    
    # export and save plots
    
    
    ggsave("firstPlot.jpeg", width = 6, height = 4)

    
    violin_plot <- ggplot(data = gapminder_1997)+
      aes(x = continent, y = lifeExp)+
      geom_violin(aes(fill = continent))

   violin_plot <- violin_plot + theme_bw()
   
   ggsave(plot = violin_plot,
          filename = "violin_sucks.jpeg",
          width = 6,
          height = 4)
   
   
   
   
   
   #animated plots
   
  install.packages(c("gganimate", "gifski"))
  library(gganimate)    
  library(gifski)   
  
  
  
  
  ggplot(data = gapminder_data) +
    aes(x = log(gdpPercap), y = lifeExp, size = pop, color = continent) +
    geom_point()
  
  
  staticHansPlot <- 
    ggplot(data = gapminder_data) +
    aes(x = log(gdpPercap), y = lifeExp, size = pop/10000000, color = continent) +
    geom_point(alpha = 0.5) +
    scale_color_brewer(palette = "Set1") +
    lab(x = "GDP Per Capita", y = "Life Expectancy", color = "Continent", size = "Population (in millions)") + 
    theme_classic()
  
  
  staticHansPlot

  
  animatedHansPlot <- staticHansPlot + 
    transition_states(year, transition_length = 1, state_length = 1) +
    ggtitle("{closest state}")  #
  
  anim_save("hansAnimatedplot.gif" , 
            plot = animatedHansPlot,
            renderer = gifski_renderer())
  
  
  
  