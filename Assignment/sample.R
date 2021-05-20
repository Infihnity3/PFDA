#MUHAMMAD ALI IFTIKHAR
#TP058489

#Assigning a variable to the Excel File called Emissions.

Emissions = read.csv(file = "C:/Users/mali1/OneDrive/Documents/4. data.csv",header = TRUE,na.strings = "")

library(dplyr)
library(ggplot2)

#Q1
#new Variable was created for the Analysis 1 Called newco1 

newco1= arrange(Emissions, year,
                Maker,
                transmission,
                transmission_type,
                fuel_type,
                co_emissions,
                nox_emissions,
                urban_imperial,
                extra_urban_imperial,
                combined_imperial) %>%
  select(year,
         Maker,
         transmission,
         transmission_type,
         fuel_type,
         co_emissions,
         nox_emissions,
         urban_imperial,
         extra_urban_imperial,
         combined_imperial) %>%
  filter(Maker %in% c("Audi-A3 Cabriolet",
                      "Audi-A6 Saloon",
                      "Vauxhall-Meriva MY2008",
                      "Vauxhall-Vectra MY2008 4 Door Saloon") & fuel_type %in% c("Diesel"))

#1.1 brands producing co_emissions
a <- ggplot(newco1,aes(x = co_emissions))
a + geom_bar(aes(fill = Maker),
             position = position_stack(reverse = TRUE), width = 2) + 
  theme(legend.position = "top") + 
  labs(title = "Brands Against producing Co Emission",
       subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
       tag = "Analysis 1.1",
       caption = "Source = newco1",
       x = "Amount of Co_emissions Emitted",
       fill = "Cars")

#1.2 brands producing nox_emissions

b <- ggplot(newco1, aes(x = nox_emissions))
b + geom_bar(aes(fill = Maker),
             position = position_stack(reverse = TRUE), width = 2) + 
  theme(legend.position = "top") + 
  labs(title = "Brands Against producing nox Emission",
       subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
       caption = "Source = newco1",
       x = " Amount Nox Emissions Emitted",
       tag = "Analysis 1.2",
       fill = "Cars") + scale_fill_brewer(palette = "Green") + 
  theme_minimal()

#1.3 brands with urban Imperial
newco1 %>%
  ggplot(mapping = aes(x = Maker, y = urban_imperial, colour = Maker)) + 
  geom_point(aes(shape = Maker)) + 
  labs (title = "Fuel efficiency for said car in City environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Cars",
        y = "Urban Imperial",
        colour = "Cars",
        tag = "Analysis 1.3") + theme_update()

#1.4 brands with extra_urban_Imperial

newco1 %>%
  ggplot(mapping = aes(x = extra_urban_imperial, y = Maker, colour = Maker)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +  
  labs (title = "Fuel efficiency for said car in off Road environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Cars",
        y = "Extra Urban Imperial",
        colour = "Cars",
        tag = "Analysis 1.4") +  theme_bw()

#1.5 brands with extra_urban_Imperial

newco1 %>%
  ggplot(mapping = aes(x = Maker, y = extra_urban_imperial, fill = Maker)) + 
  geom_boxplot(notch = FALSE) + geom_line(size = 1) + 
  labs (title = "Fuel efficiency for said car in on Road and off Road environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Cars",
        y = "Extra Urban Imperial",
        colour = "Cars",
        tag = "Analysis 1.5") + theme_bw()

#1.6 urban_imperial
newco1 %>%
  ggplot(mapping = aes(x = urban_imperial, y = co_emissions, fill = Maker)) + 
  geom_col() + 
  annotate("rect" , xmin = c(42,47) , xmax = c(45,54), ymin = c(2500,3200), ymax = c(3300,3900),
           alpha = 0.2, colour = "White", fill = "white") + # highest of Audi A3 and A6 are highlighted 
  labs (title = "Fuel efficiency for said car in on Road and off Road environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Urban Imperial",
        y = " Ammount of co_emission Emitted",
        fill = "Cars",
        tag = "Analysis 1.6") + theme_dark() 

#1.6 urban_extra_imperial
# highlighting the Audi A3/A6/VW-golf-Es/VW-golf-pl
newco1 %>%
  ggplot(mapping = aes(x = extra_urban_imperial, y = co_emissions, fill = Maker)) + 
  geom_col(width = 0.5) + 
  annotate("rect" , xmin = c(47,68,71) , xmax = c(50,70,74),
           ymin = c(2000,1800,3600), ymax = c(3800,4500,5300),
           alpha = 0.2, colour = "White", fill = "white") + 
  labs (title = "Fuel efficiency for said car in on Road and off Road environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Extra Urban Imperial",
        y = "Amount of co_emission Emitted",
        fill = "Cars",
        tag = "Analysis 1.6") + theme_dark()

#1.6 combined_imperial
# highlighting the Audi A3/A6/VW-golf-Es/VW-golf-pl

newco1 %>%
  ggplot(mapping = aes(x = combined_imperial, y = co_emissions, fill = Maker)) + 
  geom_col(width = 0.5) + 
  annotate("rect" , xmin = c(38,51,54,64.5) , xmax = c(42,54,57,67),
           ymin = c(2000,2500,1400,2100), ymax = c(2600,3300,2000,3300),
           alpha = 0.2, colour = "White", fill = "white") +
  labs (title = "Fuel efficiency for said car in on Road and off Road environment ",
        subtitle = "Audi A3 vs Audi A6 vs Volkswagen-Golf Plus vs Volkswagen-Golf Estate",
        caption = "Source = newco1",
        x =  "Combined Imperial",
        y = "Amount of co_emissions Emitted",
        fill = "Cars",
        tag = "Analysis 1.6") + theme_dark()
#---------------------------------------#-------------------------------------------------#---------------------#
#Question 2
#Does car transmission affect the amount of CO2 production of a motor vehicle

library (ggplot2)
library (dplyr)
#creating a new Variable from the Variable Emissions called cars

cars = arrange(Emissions,year,transmission,transmission_type,fuel_type,co2,nox_emissions) %>% 
  select(year,transmission,transmission_type,fuel_type,co2,nox_emissions)

#2.1 Transmission Effecting co2 production

data(cars)
View(cars)
ggplot(data = cars) + 
  geom_point(mapping = aes(x = co2, y = year, color = transmission)) + 
  labs(title = "Transmission  Effecting Co2 production" )

#2.2 Transmissions effecting co2 production yearly

ggplot(data = cars) + 
  geom_point(mapping = aes(x = co2, y = year , color = transmission)) + 
  facet_wrap(~year) + 
  labs(title = "Transmission Effecting co2 production yearly ")

#2.3 transmission types effecting Co2 production with different Fuel_Types

ggplot (data = cars) +
  geom_point(mapping = aes(x = co2, y = year, color = transmission)) +
  facet_wrap(~fuel_type) + 
  labs(title = "Transmission Effecting co2 production with various Fuel types")

#2.4 Transmission types emitting co2 production with different Transmission_types

ggplot (data = Emissions) +
  geom_point(mapping = aes(x = co2, y = year, color = transmission)) +
  facet_wrap(~transmission_type) + 
  labs(title = "Transmission Effecting Co2 with different Transmission_types") + 
  theme(legend.position = "right" )

#2.5 kia Clarus code 
Emissions %>%
  filter(Maker == "Kia-Clarus") %>%
  ggplot(aes(x = transmission, y = co2,color = transmission)) + 
  geom_line() + facet_wrap(~year) + 
  labs(title = "Kia-Clarus Effecting the production of Co2 by using Different Transmissions ")
#-------------------------------------#-----------------------------------------#----------------------#

#3 Does Makers provide a Environmental Friendly cars over the years that have been used
library(ggplot2)
library(dplyr)
#Assigning a variable to the Excel File called Emissions.

Emissions = read.csv(file = "C:/Users/mali1/OneDrive/Documents/4. data.csv",header = TRUE,na.strings = "")

# brands variable created from the data set Emissions called brands.
brands = arrange(Emissions,year,Maker,
                 transmission,
                 transmission_type,
                 fuel_type,
                 engine_capacity,
                 co2,
                 thc_emissions,
                 co_emissions,
                 nox_emissions,
                 thc_nox_emissions,
                 particulates_emissions) %>%
  select(year,
         Maker,
         transmission,
         transmission_type,
         fuel_type,
         engine_capacity,
         co2,
         thc_emissions,
         co_emissions,
         nox_emissions,
         thc_nox_emissions,
         particulates_emissions) %>%
  filter(Maker %in% c ("BMW-7 Series E38", "Aston Martin Lagonda-V8 Vantage",
                       "Vauxhall-Frontera, Model Year 2002"))

#3.1 BMW-7 Series E38 Against co2.

brands %>%
  filter(Maker == "BMW-7 Series E38") %>%
  ggplot(mapping = aes(x = Maker, y = co2 , colour = co2)) +
  geom_point() + geom_line() + 
  labs(title = "BMW-7 Series Emission of co2  in year 2000 - 2001",
       subtitle = "BMW agaisnt Co2 Emitted",
       caption = "Source = brands dataset",
       tag = "Analysis 3.1",
       x = "BMW",
       y = "Amount Co2 Emitted",
       colour = "Co2 Emission scale") + 
  facet_wrap(~year)

# 3.2 BMW-7 Series E38 against Co_emission.

brands %>%
  filter(Maker == "BMW-7 Series E38") %>%
  ggplot(mapping = aes(x = engine_capacity, y = co_emissions , color = transmission, col = "red")) +
  geom_jitter() + geom_line(size = 1) + facet_wrap(~year) +
  annotate("segment",x = 4000, xend = 4700 , y =  450, yend = 1400, colour = "Purple",
           size = 2, alpha = 0.6, arrow = arrow()) +
  labs(title = "BMW-7 series Emission in year 2000 - 2001",
       subtitle = "BMW agaisnt Co_emissions Emitted",
       caption = "Source = brands dataset",
       x = "BMW-7 Series E38",
       y = "Amount of Co2 Emitted",
       tag = "Analysis 3.2")

# 3.3 Vauxhall-Frontera, Model Year 2002 against nox_emissions. 

brands %>%
  filter(Maker == "Vauxhall-Frontera, Model Year 2002") %>%
  ggplot(mapping = aes(x = nox_emissions  , fill = transmission)) +
  geom_bar() +
  annotate("segment", x = c(200,300), xend = c(200,300), y = c(0,0), yend = c(1,3),
           size = 2, alpha = 0.3, colour = "Black", fill = "Black", arrow = arrow()) +
  facet_wrap(~year) + 
  labs(title = "Vauxhall-Frontera Transmission A4/M5 in year 2002 - 2004",
       subtitle = "Vauxhall against nox_emissions Emitted",
       caption = "Source = brands dataset",
       tag = "Analysis 3.3",
       x = " Amountc of Nox Emitted",
       colour = "Transmission")

# 3.4 Vauxhall-Frontera, Model Year 2002 against thc_emissions.

brands %>%
  filter(Maker == "Vauxhall-Frontera, Model Year 2002") %>%
  ggplot(mapping = aes(y = thc_emissions  , colour = thc_emissions)) +
  geom_bar() +
  facet_wrap(~year) +
  annotate("segment", x = 1, xend = 3, y = 75 , yend = 75, colour = "Grey",
           size = 2, alpha = 0.6, arrow = arrow()) + 
  labs(title = "Vauxhall-Frontera in year 2002 - 2004",
       subtitle = "Vauxhall against thc_emissions",
       caption = "Source = brands dataset",
       y = "Amount of Thc Emitted",
       tag = "Analysis 3.4",
       colour = "Transmission") + theme_minimal()

# 3.5 Aston Martin Lagonda-V8 Vantage against nox_emission.

brands %>%
  filter (Maker == "Aston Martin Lagonda-V8 Vantage") %>%
  ggplot(mapping = aes(x = Maker, y = nox_emissions, colour = nox_emissions)) + 
  geom_point() + geom_line(size = 1) +
  facet_wrap(~year) + 
  labs (title = "Aston Martin Nox_emissions in year 2006 - 2010",
        subtitle = "Aston Martin Lagonda-V8 Vantage",
        caption = "Source = brands",
        x = "Aston Martin",
        y = "Nox_Emission Emitted",
        tag = "Analysis 3.5",
        colour = "Nox Emissions")

# 3.6 which all brands comparison with co2.

brands %>%
  ggplot(mapping = aes (x = Maker, y = co2 ,colour = co2)) + 
  geom_point() + geom_line(size = 0.5) + 
  labs (title = "Brand Producing the highest CO2",
        subtitle = "Aston Martin Vs BMW Vs Vauxhall",
        caption = "Source = brands dataset",
        x = "Brands",
        y = "Amount of Co2 Emitted",
        tag = "Analysis 3.6",
        colour = "Co2 Emission Scale")

# 3.7 Brands VS nox_emissions.

brands %>%
  ggplot(mapping = aes (x = Maker, y = nox_emissions ,colour = nox_emissions)) + 
  geom_point() + geom_line(size = 0.5) + geom_boxplot(aes(colour = nox_emissions)) +
  labs (title = "Brand Producing the highest Amount Nox Emissons",
        subtitle = "Aston Martin Vs BMW Vs Vauxhall",
        caption = "Source = brands dataset",
        x = "Brands",
        y = "Amount of Nox Emitted",
        tag = "Analysis 3.7",
        colour = "Nox Emission Scale")

# 3.8 Brands vs thc_emissions.
brands %>%
  ggplot(mapping = aes (x = Maker, y = thc_emissions ,colour = thc_emissions)) + 
  geom_point() + geom_line(size = 0.5) + geom_boxplot(aes(colour = thc_emissions)) +
  labs (title = "Brand Producing the highest Amount thc Emissons",
        subtitle = "Aston Martin Vs BMW Vs Vauxhall",
        caption = "Source = brands dataset",
        x = "Brands",
        y = "Amount of thc Emitted",
        tag = "Analysis 3.8",
        colour = "Thc Emission Scale")

#--------------------------------------#------------------------------------------#--------------------------#
