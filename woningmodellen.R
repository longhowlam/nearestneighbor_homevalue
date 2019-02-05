#######################################################

## Wat woningen modellen

#### libraries
library(tidyverse)
library(sp)
library(maptools)
library(leaflet)

### Lees transacties in
transacties_2018 <- read_csv(
  "2018-woontransacties.csv",
  col_types = cols(
    `Datum akte` = col_date(format = "%d-%m-%Y"),
    Invoerdatum = col_date(format = "%d-%m-%Y")
  )
)

#### overzichten paar variablen ############################

parkeer = transacties_2018 %>%
  group_by(Garage) %>%  
  summarise(n=n())

Type = transacties_2018 %>% 
  group_by(`Type woning`) %>% 
  summarise(n=n())

Soort = transacties_2018 %>% 
  group_by(`Soort woning`) %>% 
  summarise(n=n())

##### Alleen woningen bekijken ################################

woningen = transacties_2018 %>% 
  filter(
    Garage == "GeenGarage"
  )

ggplot(woningen, aes(Transactieprijs)) + 
  geom_histogram(col="black", bins=50) +
  labs("Transactieprijs verdeling")

### extremen er uithalen die de boel verstoren 
woningen = woningen %>% filter(Transactieprijs < 2000000)

ggplot(woningen, aes(Woonoppervlak, `Aantal kamers`)) +
  geom_point(alpha = 0.5) 

woningen = woningen %>% 
  filter(
    Woonoppervlak < 700,
    `Aantal kamers` < 20
  )


woningen = woningen %>% 
  mutate(
    PC2 = str_sub(Postcode,1,2),
    ouderdom = 2019 - Bouwjaar
  ) %>% 
  filter(
    ouderdom < 200,
    ouderdom >= 0
  )


ggplot(woningen, aes(ouderdom)) + 
  geom_histogram(col="black", binwidth = 1) +
  labs(title = "ouderdom in jaren")

  filter(
    Woonoppervlak < 700,
    `Aantal kamers` < 20
  )

#########  h2o modellen ###############################################

