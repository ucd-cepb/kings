library(tidyverse)
library(stargazer)
library(dotenv)
library(sjPlot)

load_dot_env()

place_existance <- readRDS("EJ_DAC_Paper/Data/place_existance.RDS")

all_places <- bind_rows(place_existance) %>% 
   mutate(DAC = as.factor(DAC),
          incorporated = as.factor(incorporated),
          exists = as.factor(exists))


# glm for if DAC influences exists
exists_mod <- glm(exists~DAC*incorporated+POP+per_latino, 
                  family = binomial,
                  data = all_places)

exists_mod_2 <- glm(exists ~ MHI*incorporated+POP+per_latino, 
                    family = binomial,
                    data = all_places)

stargazer(exists_mod, exists_mod_2, type='text')

stargazer(exists_mod, exists_mod_2, type='html', out = 'EJ_DAC_Paper/Out/mods/0_exists_mod_interaction.html')

# glm for if DAC influences exists
exists_mod_3 <- glm(exists~DAC+incorporated+POP+per_latino, 
                  family = binomial,
                  data = all_places)

exists_mod_4 <- glm(exists ~ MHI+incorporated+POP+per_latino, 
                    family = binomial,
                    data = all_places)

stargazer(exists_mod_3, exists_mod_4, type='text')

stargazer(exists_mod_3, exists_mod_4, type='html', out = 'EJ_DAC_Paper/Out/mods/0_exists_mod.html')


exists_plot <- plot_model(exists_mod, type = 'int', 
                          terms = c('incorporated', 'DAC'),
                          title=" ",
                          legend.title = 'Incorporated',
                          axis.title = c('DAC', 'Exists'))+
   theme_minimal()+
   scale_x_discrete(limits=c(0,1),
                    expand=c(0.8,0)); exists_plot

# save

ggsave('EJ_DAC_Paper/Out/exists_plot.png', exists_plot, width = 4, height = 3)

           