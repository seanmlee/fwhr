library(tidyverse)

# load -------------------------------------------------------------------------
dat <- 
  read.csv(
    file = "data/dat.csv", 
    header = TRUE
  )

# factorize --------------------------------------------------------------------
dat$Sex <- 
  as.factor(dat$Sex)

# dataframes by sex ------------------------------------------------------------
female <- dat %>%
  filter(Sex == "Female")

male <- dat %>% 
  filter(Sex == "Male")

# dataframes by species --------------------------------------------------------
gbg <- dat %>%
  filter(Species == 'Gbg')

ggg <- dat %>%
  filter(Species == 'Ggg')

pts <- dat %>%
  filter(Species == 'Pts')

ptt <- dat %>%
  filter(Species == 'Ptt')

ppn <- dat %>%
  filter(Species == 'Ppn')