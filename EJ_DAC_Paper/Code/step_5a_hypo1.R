library(tidyverse)
library(stargazer)
library(dotenv)
library(sjPlot)

load_dot_env()

place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")

all_places <- bind_rows(place_existance) %>% 
   mutate(DAC = as.factor(DAC),
          incorporated = as.factor(incorporated),
          exists = as.factor(exists),
          MHI_log = log(MHI),
          POP_log = log(POP)
          )

exists_mod_3 <- glm(exists~DAC+incorporated+POP_log+per_latino, 
                  family = binomial,
                  data = all_places)

exists_mod_4 <- glm(exists ~ MHI_log+incorporated+POP_log+per_latino, 
                    family = binomial,
                    data = all_places)

stargazer( exists_mod_3, exists_mod_4, type='text')

stargazer(exists_mod_3, exists_mod_4, type='html', out = 'EJ_DAC_Paper/Out/mods/h1_existance.html')
