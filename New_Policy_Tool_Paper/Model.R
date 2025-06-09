# model
# Network -> Policy instrument paper
# loaded library package as in non_isolated.R

install.packages("flextable")
library(jtools) 
library(huxtable)
library(officer)
library(flextable)
library(lavaan)
merged_dataset <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong//network_statistics/merged_dataset_nonisolated.csv")
meta_data <- read.csv("New_Policy_Tool_Paper/Box_link/Network_policy_tool_paper/wendysong/gsp_ids_with_metadata.csv")
# mult_gsas,priority,urbangw_af,percent_dac_by_pop_scaled,fract_of_area_in_habitat,maxdryspell_scaled,Agr_Share_Of_GDP_scaled,Perc_Bach_Degree_Over25_scaled, Perc_Bach_Degree_Over25_scaled,basin_population, Republican_Vote_Share_scaled
data_meta <- merged_dataset %>%
   left_join(meta_data %>% select(gsp_id,mult_gsas,priority,exante_collab,
                                  gwsum, exceedance, basin_population, local_govs_per_10k_people, 
                                  max2012dryspell, urbangw_af_log_scaled, percent_dac_by_pop_scaled, 
                                  fract_of_area_in_habitat_log_scaled, maxdryspell_scaled, 
                                  Agr_Share_Of_GDP_scaled, Perc_Bach_Degree_Over25_scaled, 
                                  log_well_MCL_exceedance_count_by_log_pop_scaled, drywellcount2014_2020, 
                                  log_drywell_per_log_person_scaled, dsci_scaled, Republican_Vote_Share
   ), by = "gsp_id")

data_meta <- data_meta %>%
   mutate(mult_gsas = ifelse(mult_gsas == TRUE, 1, 0))
data_meta <- data_meta %>% 
   mutate(exante_collab = ifelse(exante_collab == TRUE, 1, 0))
data_meta <- data_meta %>%
   mutate(priority = case_when(
      priority == "High" ~ 4,
      priority == "Medium" ~ 3,
      priority == "Low" ~ 2,
      priority == "Very Low" ~ 1,
      TRUE ~ NA_real_ 
   ))
data <- data_meta %>%
   mutate(
      Allocations = ifelse(Allocations == 0.5, 0, Allocations),
      Trading = ifelse(Trading == 0.5, 0, Allocations),
      Taxes.Fees = ifelse(Taxes.Fees == 0.5, 0, Taxes.Fees),
      Pumping.Restrictions = ifelse(Pumping.Restrictions == 0.5, 0, Pumping.Restrictions),
      Efficiency.Incentives = ifelse(Efficiency.Incentives == 0.5, 0, Efficiency.Incentives)
   )

# instrument_type
data <- data %>%
   mutate(
      instrument_type = case_when(
         (Allocations == 1 | Pumping.Restrictions == 1) & 
            (Taxes.Fees == 1 | Efficiency.Incentives == 1) ~ 3,
         (Allocations == 1 | Pumping.Restrictions == 1) ~ 1,
         (Taxes.Fees == 1 | Efficiency.Incentives == 1) ~ 2,
         TRUE ~ 0
      ),
      instrument_type = factor(
         instrument_type,
         levels = c(0, 1, 2, 3),
         labels = c("None", "Regulatory", "Market", "Both")
      )
   )

## M(0):  Population + Multi-GSA + Exante Cooperation (Most proximate causal story)
# M(1):  M(0) + Agricultural Power
# M(2):  M(0) + Institutional Fragmentation
# M(3):  M(0) + Problem Severity
# M(4):  M(0) + Agricultural Power + Institutional Fragmentation + Problem Severity


# install.packages("jtools")  # if not already installed
library(jtools)
library(officer)

# M0 - M4
fit0 <- lm(modularity ~ basin_population + mult_gsas + exante_collab, data = data)
fit1 <- lm(modularity ~ basin_population + mult_gsas + exante_collab + Agr_Share_Of_GDP_scaled + Republican_Vote_Share, data = data)
fit2 <- lm(modularity ~ basin_population + mult_gsas + exante_collab + local_govs_per_10k_people, data = data)
fit3 <- lm(modularity ~ basin_population + mult_gsas + exante_collab + gwsum, data = data)
fit4 <- lm(modularity ~ basin_population + mult_gsas + exante_collab + Agr_Share_Of_GDP_scaled + Republican_Vote_Share + local_govs_per_10k_people + gwsum, data = data)

summary(fit0)
# Export regression 
jtools::export_summs(fit0, fit1, fit2, fit3, fit4, scale = TRUE)
export_summs(fit0, fit1, fit2, fit3, fit4, scale = TRUE,stars  = c(`.`= 0.1, `*` = 0.05, `**` = 0.01, `***` = 0.001),
             to.file = "docx", file.name = "test.docx")


# June 5
# SES -> GSA
cor(mult_gsas,)
## mult_gsas
fit0 <- glm(mult_gsas ~ gwsum + exceedance + basin_population + local_govs_per_10k_people + 
           max2012dryspell + urbangw_af_log_scaled + percent_dac_by_pop_scaled + 
           fract_of_area_in_habitat_log_scaled + maxdryspell_scaled + 
           Agr_Share_Of_GDP_scaled + Perc_Bach_Degree_Over25_scaled + 
           log_well_MCL_exceedance_count_by_log_pop_scaled + drywellcount2014_2020 + 
           log_drywell_per_log_person_scaled + dsci_scaled + Republican_Vote_Share, data = data, family = binomial())
fit1 <- glm(mult_gsas ~ basin_population  +  Agr_Share_Of_GDP_scaled + 
            Republican_Vote_Share + local_govs_per_10k_people + gwsum, data = data, family = binomial())
fit2 <- glm(mult_gsas ~ basin_population  +  Agr_Share_Of_GDP_scaled  + 
              log_drywell_per_log_person_scaled + Republican_Vote_Share + local_govs_per_10k_people + 
              gwsum, data = data, family = binomial())

summary(fit2)

## ex-ante collaboration
fit0 <- lm(exante_collab ~ gwsum + exceedance + basin_population + local_govs_per_10k_people + 
               max2012dryspell + urbangw_af_log_scaled + percent_dac_by_pop_scaled + 
               fract_of_area_in_habitat_log_scaled + maxdryspell_scaled + 
               Agr_Share_Of_GDP_scaled + Perc_Bach_Degree_Over25_scaled + 
               log_well_MCL_exceedance_count_by_log_pop_scaled + drywellcount2014_2020 + 
               log_drywell_per_log_person_scaled + dsci_scaled + Republican_Vote_Share, data = data)
fit1 <- lm(exante_collab ~ basin_population  +  Agr_Share_Of_GDP_scaled + 
               Republican_Vote_Share + local_govs_per_10k_people + gwsum, data = data)
fit2 <- lm(exante_collab ~ basin_population  +  Agr_Share_Of_GDP_scaled  + 
               log_drywell_per_log_person_scaled + Republican_Vote_Share + local_govs_per_10k_people + 
               gwsum + fract_of_area_in_habitat_log_scaled, data = data)

fit3 <- lm(cp_fit_score ~ mult_gsas + exante_collab + gwsum + exceedance + basin_population + local_govs_per_10k_people + 
              max2012dryspell + urbangw_af_log_scaled + percent_dac_by_pop_scaled + 
              fract_of_area_in_habitat_log_scaled + maxdryspell_scaled + 
              Agr_Share_Of_GDP_scaled + Perc_Bach_Degree_Over25_scaled + 
              log_well_MCL_exceedance_count_by_log_pop_scaled + drywellcount2014_2020 + 
              log_drywell_per_log_person_scaled + dsci_scaled + Republican_Vote_Share, data = data)
summary(fit3)

jtools::export_summs(fit0, fit1, fit2 , scale = TRUE)
export_summs(fit0, fit1, fit2, scale = TRUE,   
             stars  = c(`.`= 0.1, `*` = 0.05, `**` = 0.01, `***` = 0.001), 
             to.file = "docx", file.name = "SES_GSA.docx")


cor_matrix <- cor(data[, c('gwsum', 'exceedance', 'basin_population', 'local_govs_per_10k_people', 
                           'max2012dryspell', 'urbangw_af_log_scaled', 'percent_dac_by_pop_scaled', 
                           'fract_of_area_in_habitat_log_scaled', 'maxdryspell_scaled', 
                           'Agr_Share_Of_GDP_scaled', 'Perc_Bach_Degree_Over25_scaled', 
                           'log_well_MCL_exceedance_count_by_log_pop_scaled', 'drywellcount2014_2020', 
                           'log_drywell_per_log_person_scaled', 'dsci_scaled', 'Republican_Vote_Share')]
                  , data[, c('gwsum', 'exceedance', 'basin_population', 'local_govs_per_10k_people', 
                             'max2012dryspell', 'urbangw_af_log_scaled', 'percent_dac_by_pop_scaled', 
                             'fract_of_area_in_habitat_log_scaled', 'maxdryspell_scaled', 
                             'Agr_Share_Of_GDP_scaled', 'Perc_Bach_Degree_Over25_scaled', 
                             'log_well_MCL_exceedance_count_by_log_pop_scaled', 'drywellcount2014_2020', 
                             'log_drywell_per_log_person_scaled', 'dsci_scaled', 'Republican_Vote_Share')])
cor_melt <- reshape2::melt(cor_matrix)

ggplot(cor_melt, aes(x = Var2, y = Var1, fill = value)) + 
   geom_tile(color = "white") + 
   geom_text(aes(label = round(value, 2)), size = 3) + 
   scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                        name = "Correlation") + 
   theme_minimal() + 
   theme(
      axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 7),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 10, face = "bold")
   ) + 
   labs(title = "Correlation Heatmap", x = "", y = "")
      


sem_model <- '


  Ag_power =~ 
       1*Agr_Share_Of_GDP_scaled 
     + lambda_AP * Republican_Vote_Share

  Problem_severity =~ 
       1*log_drywell_per_log_person_scaled 
     + lambda_PS * gwsum


  mult_gsas ~  beta1 * Ag_power 
     + beta2 * Problem_severity 
     + beta3 * basin_population 
     + beta4 * local_govs_per_10k_people
'

fit <- sem(sem_model, 
           data      = data, 
           estimator = "MLR",    
           missing   = "FIML")   

summary(fit, 
        standardized = TRUE, 
        fit.measures  = TRUE, 
        rsquare       = TRUE)



cfa_ps <- '
  Problem_severity =~ max2012dryspell + gwsum
'
fit_ps <- cfa(cfa_ps, data = data, estimator = "MLR")
summary(fit_ps, standardized=TRUE, fit.measures=TRUE)

# make two vars in data, called a and b
data$a <- rnorm(nrow(data))
data$b <- rnorm(nrow(data))
data$c <- rnorm(nrow(data))

cfa_ps <- '
  Problem_severity =~ a + b + c
'
fit_ps <- cfa(cfa_ps, data = data)

summary(fit_ps)


varTable(fit)

cor_test_ps <- cor.test(
   data$log_drywell_per_log_person_scaled,
   data$gwsum,
   use    = "pairwise.complete.obs",
   method = "pearson"
)
print(cor_test_ps)



vars_ps <- c("log_drywell_per_log_person_scaled", "gwsum")

cfa_col <- '
  eta_COL =~ 1*MultiGSA + lambda_COL*ExAnteCollab
'
fit_col <- cfa(cfa_col, data = data, estimator = "MLR")
summary(fit_col, standardized=TRUE, fit.measures=TRUE)

