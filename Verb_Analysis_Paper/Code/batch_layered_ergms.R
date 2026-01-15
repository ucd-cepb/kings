
library(statnet)
library(ergm.multi)
library(foreach)
library(doParallel)

# ============================================================================
# TEST MODE CONFIGURATION
# ============================================================================
# Set TEST_MODE = TRUE to run a single model without parallelization for debugging
# This helps identify errors before running the full batch
TEST_MODE <- FALSE
TEST_INDEX <- 35  # Which model to test (35 is noted as a short/quick test case)

# ============================================================================
# MCMC / FITTING CONFIGURATION
# ============================================================================
# Main fitting method: "Stochastic-Approximation" (robust) or "MCMLE" (faster)
MAIN_METHOD <- "Stochastic-Approximation"

# MCMC parameters scale with network size: base_value * (n_nodes / 100)
# Set to 1 for fixed params, >1 for more conservative scaling
MCMC_SCALE_FACTOR <- 1.0

# Base MCMC parameters (will be scaled by network size if MCMC_SCALE_FACTOR > 0)
MCMC_BASE_BURNIN     <- 50e3
MCMC_BASE_SAMPLESIZE <- 50e3
MCMC_BASE_INTERVAL   <- 2500

# Directory for incremental saves (prevents losing progress on crash)
INCREMENTAL_SAVE_DIR <- "data/Verb_Analysis_Paper/incremental_fits"

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
# HELPER FUNCTIONS
# ============================================================================

# Compute MCMC parameters scaled by network size
get_mcmc_params <- function(n_nodes) {
   scale <- if(MCMC_SCALE_FACTOR > 0) max(1, (n_nodes / 100) * MCMC_SCALE_FACTOR) else 1
   list(
      burnin     = as.integer(MCMC_BASE_BURNIN * scale),
      samplesize = as.integer(MCMC_BASE_SAMPLESIZE * scale),
      interval   = as.integer(MCMC_BASE_INTERVAL * scale)
   )
}

# Save individual model result incrementally
save_incremental <- function(fit, model_type, index) {
   if(!dir.exists(INCREMENTAL_SAVE_DIR)) dir.create(INCREMENTAL_SAVE_DIR, recursive = TRUE)
   fname <- file.path(INCREMENTAL_SAVE_DIR, sprintf("%s_%03d.RDS", model_type, index))
   saveRDS(fit, fname)
}

# Load any incremental saves and merge into results list
load_incremental <- function(model_type, results_list) {
   if(!dir.exists(INCREMENTAL_SAVE_DIR)) return(results_list)
   pattern <- sprintf("^%s_\\d{3}\\.RDS$", model_type)
   files <- list.files(INCREMENTAL_SAVE_DIR, pattern = pattern, full.names = TRUE)
   for(f in files) {
      idx <- as.integer(gsub(".*_(\\d{3})\\.RDS$", "\\1", basename(f)))
      if(is.null(results_list[[idx]])) {
         results_list[[idx]] <- readRDS(f)
      }
   }
   results_list
}

# Clean up incremental files after successful batch save
cleanup_incremental <- function(model_type) {
   if(!dir.exists(INCREMENTAL_SAVE_DIR)) return()
   pattern <- sprintf("^%s_\\d{3}\\.RDS$", model_type)
   files <- list.files(INCREMENTAL_SAVE_DIR, pattern = pattern, full.names = TRUE)
   file.remove(files)
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

# Load any incremental saves from previous interrupted runs
m0s <- load_incremental("m0", m0s)
m1s <- load_incremental("m1", m1s)
m2s <- load_incremental("m2", m2s)

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

      # ========== TEST MODE: Run single model without parallelization ==========
      if(TEST_MODE) {
         k <- TEST_INDEX
         n_nodes <- network.size(layered_nets[[k]])
         mcmc_params <- get_mcmc_params(n_nodes)

         cat(sprintf("\n========== TEST MODE: Fitting model %d (m0) ==========\n", k))
         cat(sprintf("Network has %d nodes\n", n_nodes))
         cat(sprintf("MCMC params: burnin=%d, interval=%d, samplesize=%d\n",
                     mcmc_params$burnin, mcmc_params$interval, mcmc_params$samplesize))
         cat(sprintf("Main method: %s\n", MAIN_METHOD))

         f0_local <- layered_nets[[k]] ~
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         cat("Formula:\n")
         print(f0_local)
         cat("\n")

         # Stage 1: MPLE
         cat("Stage 1: MPLE fit...\n")
         control_mple = control.ergm(
            parallel = threads_per_model,
            main.method = 'MPLE',
            init.method = 'MPLE'
         )
         mple_fit <- ergm(f0_local, control = control_mple)
         cat("MPLE coefficients:\n")
         print(coef(mple_fit))
         cat("\n")

         # Stage 2: Main method fit
         cat(sprintf("Stage 2: %s fit...\n", MAIN_METHOD))
         control_main = control.ergm(
            parallel = threads_per_model,
            main.method = MAIN_METHOD,
            init = coef(mple_fit),
            MCMC.burnin = mcmc_params$burnin,
            MCMC.interval = mcmc_params$interval,
            MCMC.samplesize = mcmc_params$samplesize,
            MCMLE.conv.min.pval = 0.2
         )
         fit <- ergm(f0_local, control = control_main)

         cat("\n========== TEST MODE: Model 0 Results ==========\n")
         print(summary(fit))
         cat("\n========== TEST MODE COMPLETE ==========\n")
         cat("Set TEST_MODE <- FALSE to run full batch.\n\n")

      } else {
         # ========== NORMAL MODE: Parallel batch processing ==========
         cat(sprintf("Fitting %d base models (m0) in parallel...\n", length(models_to_fit)))

         # Set up parallel cluster for outer parallelization
         cl <- makeCluster(parallel_models)
         registerDoParallel(cl)

         # Export config to workers
         clusterExport(cl, c("MAIN_METHOD", "MCMC_SCALE_FACTOR", "MCMC_BASE_BURNIN",
                            "MCMC_BASE_SAMPLESIZE", "MCMC_BASE_INTERVAL",
                            "get_mcmc_params", "save_incremental", "INCREMENTAL_SAVE_DIR"))

         # Fit models in parallel
         results <- foreach(k = models_to_fit,
                           .packages = c('statnet', 'ergm.multi'),
                           .errorhandling = 'pass') %dopar% {
            cat(sprintf("Fitting model %d (m0)\n", k))

            # Define formula for this model
            f0_local <- layered_nets[[k]] ~
               L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

            # Get network-scaled MCMC params
            n_nodes <- network.size(layered_nets[[k]])
            mcmc_params <- get_mcmc_params(n_nodes)

            tryCatch({
               # Stage 1: Fit with MPLE to get initial values
               cat(sprintf("  Model %d: Stage 1 - MPLE fit\n", k))
               control_mple = control.ergm(
                  parallel = threads_per_model,
                  main.method = 'MPLE',
                  init.method = 'MPLE'
               )
               mple_fit <- ergm(f0_local, control = control_mple)

               # Stage 2: Fit with configured method using MPLE coefficients as init
               cat(sprintf("  Model %d: Stage 2 - %s fit\n", k, MAIN_METHOD))
               control_main = control.ergm(
                  parallel = threads_per_model,
                  main.method = MAIN_METHOD,
                  init = coef(mple_fit),
                  MCMC.burnin = mcmc_params$burnin,
                  MCMC.interval = mcmc_params$interval,
                  MCMC.samplesize = mcmc_params$samplesize,
                  MCMLE.conv.min.pval = 0.2
               )
               fit <- ergm(f0_local, control = control_main)

               # Save incrementally
               save_incremental(fit, "m0", k)

               cat(sprintf("  Model %d: Complete\n", k))
               return(fit)
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

         # Save after all parallel fits complete and clean up incremental files
         saveRDS(m0s, mod0_saved)
         cleanup_incremental("m0")
         cat("Base models (m0) saved.\n")
      }
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

      # ========== TEST MODE: Run single model without parallelization ==========
      if(TEST_MODE) {
         k <- TEST_INDEX
         n_nodes <- network.size(layered_nets[[k]])
         mcmc_params <- get_mcmc_params(n_nodes)

         cat(sprintf("\n========== TEST MODE: Fitting model %d (m1) ==========\n", k))
         cat(sprintf("Network has %d nodes\n", n_nodes))
         cat(sprintf("MCMC params: burnin=%d, interval=%d, samplesize=%d\n",
                     mcmc_params$burnin, mcmc_params$interval, mcmc_params$samplesize))
         cat(sprintf("Main method: %s\n", MAIN_METHOD))

         f0_local <- layered_nets[[k]] ~
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         f1_local <- update.formula(f0_local, ~ . +
                                    L(~edges, ~`info`& ( `money` | `neither`)) +
                                    L(~edges, ~`money`& (`info` | `neither`)))

         cat("Formula:\n")
         print(f1_local)
         cat("\n")

         # Warm-start from m0 if available
         if(!is.null(m0s[[k]]) && inherits(m0s[[k]], "ergm")) {
            init_vals <- c(coef(m0s[[k]]), 0, 0)
            cat("Warm-starting from m0 coefficients:\n")
            print(coef(m0s[[k]]))
         } else {
            cat("Stage 1: MPLE fit (m0 not available)...\n")
            control_mple = control.ergm(
               parallel = threads_per_model,
               main.method = 'MPLE',
               init.method = 'MPLE'
            )
            mple_fit <- ergm(f1_local, control = control_mple)
            init_vals <- coef(mple_fit)
            cat("MPLE coefficients:\n")
            print(init_vals)
         }
         cat("\n")

         # Stage 2: Main method fit
         cat(sprintf("Stage 2: %s fit...\n", MAIN_METHOD))
         control_main = control.ergm(
            parallel = threads_per_model,
            main.method = MAIN_METHOD,
            init = init_vals,
            MCMC.burnin = mcmc_params$burnin,
            MCMC.interval = mcmc_params$interval,
            MCMC.samplesize = mcmc_params$samplesize,
            MCMLE.conv.min.pval = 0.2
         )
         fit <- ergm(f1_local, control = control_main)

         cat("\n========== TEST MODE: Model 1 Results ==========\n")
         print(summary(fit))
         cat("\n========== TEST MODE COMPLETE ==========\n")
         cat("Set TEST_MODE <- FALSE to run full batch.\n\n")

      } else {
         # ========== NORMAL MODE: Parallel batch processing ==========
         cat(sprintf("Fitting %d reciprocity models (m1) in parallel...\n", length(models_to_fit)))

         # Set up parallel cluster for outer parallelization
         cl <- makeCluster(parallel_models)
         registerDoParallel(cl)

         # Export config and m0s (for warm-starting) to workers
         clusterExport(cl, c("MAIN_METHOD", "MCMC_SCALE_FACTOR", "MCMC_BASE_BURNIN",
                            "MCMC_BASE_SAMPLESIZE", "MCMC_BASE_INTERVAL",
                            "get_mcmc_params", "save_incremental", "INCREMENTAL_SAVE_DIR", "m0s"))

         # Fit models in parallel
         results <- foreach(k = models_to_fit,
                           .packages = c('statnet', 'ergm.multi'),
                           .errorhandling = 'pass') %dopar% {
            cat(sprintf("Fitting model %d (m1)\n", k))

            # Define formulas for this model
            f0_local <- layered_nets[[k]] ~
               L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

            f1_local <- update.formula(f0_local, ~ . +
                                       L(~edges, ~`info`& ( `money` | `neither`)) +
                                       L(~edges, ~`money`& (`info` | `neither`)))

            # Get network-scaled MCMC params
            n_nodes <- network.size(layered_nets[[k]])
            mcmc_params <- get_mcmc_params(n_nodes)

            # Warm-start: use m0 coefficients if available, else use MPLE
            if(!is.null(m0s[[k]]) && inherits(m0s[[k]], "ergm")) {
               init_vals <- c(coef(m0s[[k]]), 0, 0)  # m0 has 3 terms, m1 adds 2
               cat(sprintf("  Model %d: Warm-starting from m0 coefficients\n", k))
            } else {
               control_mple = control.ergm(
                  parallel = threads_per_model,
                  main.method = 'MPLE',
                  init.method = 'MPLE'
               )
               mple_fit <- ergm(f1_local, control = control_mple)
               init_vals <- coef(mple_fit)
               cat(sprintf("  Model %d: Using MPLE initialization (m0 not available)\n", k))
            }

            tryCatch({
               cat(sprintf("  Model %d: Fitting with %s\n", k, MAIN_METHOD))
               control_main = control.ergm(
                  parallel = threads_per_model,
                  main.method = MAIN_METHOD,
                  init = init_vals,
                  MCMC.burnin = mcmc_params$burnin,
                  MCMC.interval = mcmc_params$interval,
                  MCMC.samplesize = mcmc_params$samplesize,
                  MCMLE.conv.min.pval = 0.2
               )
               fit <- ergm(f1_local, control = control_main)

               # Save incrementally
               save_incremental(fit, "m1", k)

               cat(sprintf("  Model %d: Complete\n", k))
               return(fit)
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

         # Save after all parallel fits complete and clean up incremental files
         saveRDS(m1s, mod1_saved)
         cleanup_incremental("m1")
         cat("Reciprocity models (m1) saved.\n")
      }
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

      # ========== TEST MODE: Run single model without parallelization ==========
      if(TEST_MODE) {
         k <- TEST_INDEX
         n_nodes <- network.size(layered_nets[[k]])
         mcmc_params <- get_mcmc_params(n_nodes)

         cat(sprintf("\n========== TEST MODE: Fitting model %d (m2) ==========\n", k))
         cat(sprintf("Network has %d nodes\n", n_nodes))
         cat(sprintf("MCMC params: burnin=%d, interval=%d, samplesize=%d\n",
                     mcmc_params$burnin, mcmc_params$interval, mcmc_params$samplesize))
         cat(sprintf("Main method: %s\n", MAIN_METHOD))

         f0_local <- layered_nets[[k]] ~
            L(~edges, ~`info`) + L(~edges, ~`money`) + L(~edges, ~`neither`)

         f1_local <- update.formula(f0_local, ~ . +
                                    L(~edges, ~`info`& ( `money` | `neither`)) +
                                    L(~edges, ~`money`& (`info` | `neither`)))

         f2_local <- update.formula(f1_local, ~ . +
                                    gwespL(L.base = ~`info`, Ls.path = list(~`money`|`neither`|`info`, ~`money`|`neither`|`info`), decay = 1, fixed = T) +
                                    gwespL(L.base = ~`money`, Ls.path = list(~`info`|`neither`|`money`, ~`info`|`neither`|`money`), decay = 1, fixed = T))

         cat("Formula:\n")
         print(f2_local)
         cat("\n")

         # Warm-start from m1 if available
         if(!is.null(m1s[[k]]) && inherits(m1s[[k]], "ergm")) {
            init_vals <- c(coef(m1s[[k]]), 0, 0)
            cat("Warm-starting from m1 coefficients:\n")
            print(coef(m1s[[k]]))
         } else {
            cat("Stage 1: MPLE fit (m1 not available)...\n")
            control_mple = control.ergm(
               parallel = threads_per_model,
               main.method = 'MPLE',
               init.method = 'MPLE'
            )
            mple_fit <- ergm(f2_local, control = control_mple)
            init_vals <- coef(mple_fit)
            cat("MPLE coefficients:\n")
            print(init_vals)
         }
         cat("\n")

         # Stage 2: Main method fit
         cat(sprintf("Stage 2: %s fit...\n", MAIN_METHOD))
         control_main = control.ergm(
            parallel = threads_per_model,
            main.method = MAIN_METHOD,
            init = init_vals,
            MCMC.burnin = mcmc_params$burnin,
            MCMC.interval = mcmc_params$interval,
            MCMC.samplesize = mcmc_params$samplesize,
            MCMLE.conv.min.pval = 0.2
         )
         fit <- ergm(f2_local, control = control_main)

         cat("\n========== TEST MODE: Model 2 Results ==========\n")
         print(summary(fit))
         cat("\n========== TEST MODE COMPLETE ==========\n")
         cat("Set TEST_MODE <- FALSE to run full batch.\n\n")

      } else {
         # ========== NORMAL MODE: Parallel batch processing ==========
         cat(sprintf("Fitting %d transitivity models (m2) in parallel...\n", length(models_to_fit)))

         # Set up parallel cluster for outer parallelization
         cl <- makeCluster(parallel_models)
         registerDoParallel(cl)

         # Export config and m1s (for warm-starting) to workers
         clusterExport(cl, c("MAIN_METHOD", "MCMC_SCALE_FACTOR", "MCMC_BASE_BURNIN",
                            "MCMC_BASE_SAMPLESIZE", "MCMC_BASE_INTERVAL",
                            "get_mcmc_params", "save_incremental", "INCREMENTAL_SAVE_DIR", "m1s"))

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

            # Get network-scaled MCMC params
            n_nodes <- network.size(layered_nets[[k]])
            mcmc_params <- get_mcmc_params(n_nodes)

            # Warm-start: use m1 coefficients if available, else use MPLE
            if(!is.null(m1s[[k]]) && inherits(m1s[[k]], "ergm")) {
               init_vals <- c(coef(m1s[[k]]), 0, 0)  # m1 has 5 terms, m2 adds 2
               cat(sprintf("  Model %d: Warm-starting from m1 coefficients\n", k))
            } else {
               control_mple = control.ergm(
                  parallel = threads_per_model,
                  main.method = 'MPLE',
                  init.method = 'MPLE'
               )
               mple_fit <- ergm(f2_local, control = control_mple)
               init_vals <- coef(mple_fit)
               cat(sprintf("  Model %d: Using MPLE initialization (m1 not available)\n", k))
            }

            tryCatch({
               cat(sprintf("  Model %d: Fitting with %s\n", k, MAIN_METHOD))
               control_main = control.ergm(
                  parallel = threads_per_model,
                  main.method = MAIN_METHOD,
                  init = init_vals,
                  MCMC.burnin = mcmc_params$burnin,
                  MCMC.interval = mcmc_params$interval,
                  MCMC.samplesize = mcmc_params$samplesize,
                  MCMLE.conv.min.pval = 0.2
               )
               fit <- ergm(f2_local, control = control_main)

               # Save incrementally
               save_incremental(fit, "m2", k)

               cat(sprintf("  Model %d: Complete\n", k))
               return(fit)
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

         # Save after all parallel fits complete and clean up incremental files
         saveRDS(m2s, mod2_saved)
         cleanup_incremental("m2")
         cat("Transitivity models (m2) saved.\n")
      }
   }
}




