library(tidyverse)
library(here)
library(arrow)
library(janitor)

## 2015

teds_d_2015 <- read_csv(here("data/tedsd_2015_puf.csv"))

# ensure year and count are correct
table(teds_d_2015$DISYR)

## 2016

teds_d_2016 <- read_csv(here("data/tedsd_2016_puf.csv"))

# ensure year and count are correct
table(teds_d_2016$DISYR)


## 2017

teds_d_2017 <- read_csv(here("data/tedsd_puf_2017.csv"))

# ensure year and count are correct
table(teds_d_2017$DISYR)


## 2018

teds_d_2018 <- read_csv(here("data/tedsd_puf_2018.csv"))

# ensure year and count are correct
table(teds_d_2018$DISYR)


## 2019

teds_d_2019 <- read_csv(here("data/tedsd_puf_2019.csv"))

# ensure year and count are correct
table(teds_d_2019$DISYR)



## Join data
teds_d_15_19 <- bind_rows(teds_d_2015, teds_d_2016, teds_d_2017, teds_d_2018, teds_d_2019)

## Check year
table(teds_d_15_19$DISYR)

## Clean names

teds_d_15_19 <- teds_d_15_19 %>% 
  clean_names()


## View

head(teds_d_15_19)


## Convert -9 to NA

teds_d_15_19[teds_d_15_19 == "-9"] <- NA



## View

head(teds_d_15_19)


## Write as feather file

write_feather(teds_d_15_19, here("data/teds_d_15_19.feather"))




