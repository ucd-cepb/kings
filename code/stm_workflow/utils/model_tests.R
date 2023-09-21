#ok
prep1 <- estimateEffect(1:50 ~ admin + 
                           basin_plan +
                           sust_criteria +
                           monitoring_networks + 
                           projects_mgmt_actions, 
                        gsp_model_saved,
                        meta = gsp_out$meta, uncertainty = "None")
#ok
prep <- estimateEffect(1:50 ~ admin + 
                          basin_plan +
                          sust_criteria +
                          monitoring_networks + 
                          projects_mgmt_actions + 
                          #as.factor(gsp_id) +
                          SVI_na_adj+
                          as.factor(approval) +
                          as.factor(priority)+ 
                          ag_gw_asfractof_tot_gw, 
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#singular
prep <- estimateEffect(1:50 ~ admin + 
                          basin_plan +
                          sust_criteria +
                          monitoring_networks + 
                          projects_mgmt_actions + 
                          as.factor(gsp_id) +
                          #SVI_na_adj+
                          as.factor(approval) +
                          as.factor(priority),
                       # ag_gw_asfractof_tot_gw
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#ok
prep <- estimateEffect(1:50 ~ admin + 
                          basin_plan +
                          sust_criteria +
                          monitoring_networks + 
                          projects_mgmt_actions + 
                          as.factor(gsp_id),
                       #SVI_na_adj+
                       #as.factor(approval) +
                       #as.factor(priority),
                       # ag_gw_asfractof_tot_gw
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#ok
prep <- estimateEffect(1:50 ~ as.factor(gsp_id),
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#singular
prep <- estimateEffect(1:50 ~ as.factor(gsp_id) +
                          SVI_na_adj,
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#singular
prep <- estimateEffect(1:50 ~ as.factor(gsp_id) +
                          #SVI_na_adj+
                          as.factor(approval),
                       # ag_gw_asfractof_tot_gw
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#singular
prep <- estimateEffect(1:50 ~ as.factor(gsp_id) +
                          #SVI_na_adj+
                          as.factor(priority),
                       # ag_gw_asfractof_tot_gw
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")

#singular
prep <- estimateEffect(1:50 ~ as.factor(gsp_id) +
                          #SVI_na_adj+
                          ag_gw_asfractof_tot_gw,
                       gsp_model_saved,
                       meta = gsp_out$meta, uncertainty = "None")