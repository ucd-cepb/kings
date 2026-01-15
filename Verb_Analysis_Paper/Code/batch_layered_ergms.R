
library(statnet)
library(ergm.multi)
library(foreach)
library(doParallel)

# ============================================================================
# NESTED PARALLELIZATION CONFIGURATION
# ============================================================================
# Detect total available cores
total_cores <- parallel::detectCores()

# Set parallelization parameters
# parallel_models: number of models to fit simultaneously (outer parallelization)
# threads_per_model: number of threads each ERGM model uses (inner parallelization)
parallel_models <- 15     # Adjust this value
threads_per_model <- 16   # Adjust this value

# Validate that we don't exceed available cores
total_cores_requested <- parallel_models * threads_per_model
if(total_cores_requested > total_cores) {
  warning(sprintf(
    "Requested cores (%d models × %d threads = %d) exceeds available cores (%d).\nAdjusting to fit within limits.",
    parallel_models, threads_per_model, total_cores_requested, total_cores
  ))

  # Adjust parameters to fit within available cores
  # Prioritize more models over threads per model
  if(parallel_models > total_cores) {
    parallel_models <- total_cores
    threads_per_model <- 1
  } else {
    threads_per_model <- floor(total_cores / parallel_models)
  }

  cat(sprintf("Adjusted to: %d models × %d threads = %d cores\n",
              parallel_models, threads_per_model, parallel_models * threads_per_model))
} else {
  cat(sprintf("Using: %d models × %d threads = %d cores (of %d available)\n",
              parallel_models, threads_per_model, total_cores_requested, total_cores))
}

# ============================================================================

layered_nets <- readRDS("data/Verb_Analysis_Paper/layered_nets.RDS")

#not going to use this table to specify different models for different plans
#summdf <- readRDS("data/Verb_Analysis_Paper/reciprocal_edge_summary_by_plan.RDS")

#the 35th plan is short, use this as test case
#k = 35
mod0_saved = 'data/Verb_Analysis_Paper/model_0_results.RDS'
mod1_saved = 'data/Verb_Analysis_Paper/model_1_results.RDS'
mod2_saved = 'data/Verb_Analysis_Paper/model_2_results.RDS'
if(file.exists(mod0_saved)){m0s <- readRDS(mod0_saved)}else{m0s <- vector(mode = "list", length = length(layered_nets))}
if(file.exists(mod1_saved)){m1s <- readRDS(mod1_saved)}else{m1s <- vector(mode = "list", length = length(layered_nets))}
if(file.exists(mod2_saved)){m2s <- readRDS(mod2_saved)}else{m2s <- vector(mode = "list", length = length(layered_nets))}

#control = control.ergm(main.method = "Stochastic-Approximation",MPLE.maxit = 1e3,MPLE.covariance.sim.burnin = 500,
#                       MPLE.covariance.samplesize = 200,MPLE.nonident = 'warning',init.method = 'MPLE')
# Note: control object is now defined locally within each foreach loop
# to use the current threads_per_model value

# Formula templates (actual formulas are created within foreach loops for each k)
# f0: Base model with edges by layer
#     L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)
#
# f1: Adds pairings of ties between layers (undirected)
#     + L(~edges, ~`info`& ( `money` | `neither`))
#     + L(~edges, ~`money`& (`info` | `neither`))
#
# f2: Adds transitivity terms (gwespL)
#     + gwespL(L.base = ~`info`, Ls.path = list(~`money`|`neither`|`info`, ~`money`|`neither`|`info`), decay = 1, fixed = T)
#     + gwespL(L.base = ~`money`, Ls.path = list(~`info`|`neither`|`money`, ~`info`|`neither`|`money`), decay = 1, fixed = T)


if(all(sapply(m0s,class)=='ergm')){
   print('all base models have finished')
}else{
   # Find which models still need to be fit
   models_to_fit <- which(sapply(m0s, is.null))

   if(length(models_to_fit) > 0) {
      cat(sprintf("Fitting %d base models (m0) in parallel...\n", length(models_to_fit)))

      # Set up parallel cluster for outer parallelization
      cl <- makeCluster(parallel_models)
      registerDoParallel(cl)

      # Fit models in parallel
      results <- foreach(k = models_to_fit,
                        .packages = c('statnet', 'ergm.multi'),
                        .errorhandling = 'pass') %dopar% {
         cat(sprintf("Fitting model %d (m0)\n", k))
         #mnet <- ergm.multi::combine_networks(net_list,blockID.vattr = 'layer')

         # Define formula for this model
         f0_local <- layered_nets[[k]] ~
            #density by layer
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         tryCatch({
            # Stage 1: Fit with MPLE to get initial values
            cat(sprintf("  Model %d: Stage 1 - MPLE fit\n", k))
            control_mple = control.ergm(
               parallel = threads_per_model,
               main.method = 'MPLE',
               init.method = 'MPLE'
            )
            mple_fit <- ergm(f0_local, control = control_mple)

            # Stage 2: Fit with Stochastic Approximation using MPLE coefficients as init
            cat(sprintf("  Model %d: Stage 2 - Stochastic Approximation fit\n", k))
            control_sa = control.ergm(
               parallel = threads_per_model,
               main.method = 'Stochastic-Approximation',
               init = coef(mple_fit),
               MPLE.samplesize = 100e3,
               MCMC.burnin = 100e3,
               MCMC.interval = 5e3,
               MCMC.samplesize = 100e3,
               MCMLE.conv.min.pval = 0.2
            )
            sa_fit <- ergm(f0_local, control = control_sa)

            cat(sprintf("  Model %d: Complete\n", k))
            return(sa_fit)
         },
         error = function(e) {
            cat(sprintf("Error in model %d: %s\n", k, e$message))
            return(NULL)
         })
      }

      # Stop the cluster
      stopCluster(cl)

      # Store results back into m0s
      for(i in seq_along(models_to_fit)) {
         m0s[[models_to_fit[i]]] <- results[[i]]
      }

      # Save after all parallel fits complete
      saveRDS(m0s, mod0_saved)
      cat("Base models (m0) saved.\n")
   }
}   
   
if(all(sapply(m1s,class)=='ergm')){
   print('all reciprocity models have finished')
}else{
   #if(summdf[k,"info_and_neither"] > 0 | summdf[k,"money_and_info"] > 0){} ALWAYS TRUE WITH OUR DATA
   #not going to invoke logic for reciprocity terms after all
   #if(summdf[k, "money_and_info"] > 0 | summdf[k, "money_and_neither"] > 0){
   #ok to add recip_money
   #}

   # Find which models still need to be fit
   models_to_fit <- which(sapply(m1s, is.null))

   if(length(models_to_fit) > 0) {
      cat(sprintf("Fitting %d reciprocity models (m1) in parallel...\n", length(models_to_fit)))

      # Set up parallel cluster for outer parallelization
      cl <- makeCluster(parallel_models)
      registerDoParallel(cl)

      # Fit models in parallel
      results <- foreach(k = models_to_fit,
                        .packages = c('statnet', 'ergm.multi'),
                        .errorhandling = 'pass') %dopar% {
         cat(sprintf("Fitting model %d (m1)\n", k))

         # Define formulas for this model
         f0_local <- layered_nets[[k]] ~
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         f1_local <- update.formula(f0_local, ~ . +
                                    # pairings of ties between layers (undirected)
                                    L(~edges, ~`info`& ( `money` | `neither`)) +
                                    L(~edges, ~`money`& (`info` | `neither`)))

         tryCatch({
            # Stage 1: Fit with MPLE to get initial values
            cat(sprintf("  Model %d: Stage 1 - MPLE fit\n", k))
            control_mple = control.ergm(
               parallel = threads_per_model,
               main.method = 'MPLE',
               init.method = 'MPLE'
            )
            mple_fit <- ergm(f1_local, control = control_mple)

            # Stage 2: Fit with Stochastic Approximation using MPLE coefficients as init
            cat(sprintf("  Model %d: Stage 2 - Stochastic Approximation fit\n", k))
            control_sa = control.ergm(
               parallel = threads_per_model,
               main.method = 'Stochastic-Approximation',
               init = coef(mple_fit),
               MPLE.samplesize = 100e3,
               MCMC.burnin = 100e3,
               MCMC.interval = 5e3,
               MCMC.samplesize = 100e3,
               MCMLE.conv.min.pval = 0.2
            )
            sa_fit <- ergm(f1_local, control = control_sa)

            cat(sprintf("  Model %d: Complete\n", k))
            return(sa_fit)
         },
         error = function(e) {
            cat(sprintf("Error in model %d: %s\n", k, e$message))
            return(NULL)
         })
      }

      # Stop the cluster
      stopCluster(cl)

      # Store results back into m1s
      for(i in seq_along(models_to_fit)) {
         m1s[[models_to_fit[i]]] <- results[[i]]
      }

      # Save after all parallel fits complete
      saveRDS(m1s, mod1_saved)
      cat("Reciprocity models (m1) saved.\n")
   }
}
      ### NOTE THAT I WOULD USUALLY START WITH DECAY = 0 AND HEAT IT UP
      ### BUT DECAY = 0 CREATES NaNs here
      ### what this should be doing is edgewise shared partners for a an edge in layer X build by ties in any the other layers
      ### so like, layer 1 tie incident on A--B where there is a layer 2 or layer 3 tie from A--C and B--C
if(all(sapply(m2s,class)=='ergm')){
   print('all transitivity models have finished')
}else{
   # Find which models still need to be fit
   models_to_fit <- which(sapply(m2s, is.null))

   if(length(models_to_fit) > 0) {
      cat(sprintf("Fitting %d transitivity models (m2) in parallel...\n", length(models_to_fit)))

      # Set up parallel cluster for outer parallelization
      cl <- makeCluster(parallel_models)
      registerDoParallel(cl)

      # Fit models in parallel
      results <- foreach(k = models_to_fit,
                        .packages = c('statnet', 'ergm.multi'),
                        .errorhandling = 'pass') %dopar% {
         cat(sprintf("Fitting model %d (m2)\n", k))

         # Define formulas for this model
         f0_local <- layered_nets[[k]] ~
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         f1_local <- update.formula(f0_local, ~ . +
                                    L(~edges, ~`info`& ( `money` | `neither`)) +
                                    L(~edges, ~`money`& (`info` | `neither`)))

         f2_local <- update.formula(f1_local, ~ . +
                                    gwespL(L.base = ~`info`, Ls.path = list(~`money`|`neither`|`info`, ~`money`|`neither`|`info`), decay = 1, fixed = T) +
                                    gwespL(L.base = ~`money`, Ls.path = list(~`info`|`neither`|`money`, ~`info`|`neither`|`money`), decay = 1, fixed = T))

         tryCatch({
            # Stage 1: Fit with MPLE to get initial values
            cat(sprintf("  Model %d: Stage 1 - MPLE fit\n", k))
            control_mple = control.ergm(
               parallel = threads_per_model,
               main.method = 'MPLE',
               init.method = 'MPLE'
            )
            mple_fit <- ergm(f2_local, control = control_mple)

            # Stage 2: Fit with Stochastic Approximation using MPLE coefficients as init
            cat(sprintf("  Model %d: Stage 2 - Stochastic Approximation fit\n", k))
            control_sa = control.ergm(
               parallel = threads_per_model,
               main.method = 'Stochastic-Approximation',
               init = coef(mple_fit),
               MPLE.samplesize = 100e3,
               MCMC.burnin = 100e3,
               MCMC.interval = 5e3,
               MCMC.samplesize = 100e3,
               MCMLE.conv.min.pval = 0.2
            )
            sa_fit <- ergm(f2_local, control = control_sa)

            cat(sprintf("  Model %d: Complete\n", k))
            return(sa_fit)
         },
         error = function(e) {
            cat(sprintf("Error in model %d: %s\n", k, e$message))
            return(NULL)
         })
      }

      # Stop the cluster
      stopCluster(cl)

      # Store results back into m2s
      for(i in seq_along(models_to_fit)) {
         m2s[[models_to_fit[i]]] <- results[[i]]
      }

      # Save after all parallel fits complete
      saveRDS(m2s, mod2_saved)
      cat("Transitivity models (m2) saved.\n")
   }
}




