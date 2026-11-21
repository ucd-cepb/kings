# model_tables.R
# Word (docx) tables for the paper and appendix, for upload to Google Docs.
#   - coefficients with SEs, stars as superscript (* .05, ** .01, *** .001)
#   - bottom rows: plan random-intercept SD, dispersion (NB only), observations, plans
# Sourced by step_6 and step_7: make_table(mods) for models, style_table(ft) for any other table,
# then flextable::save_as_docx().

library(modelsummary)
library(flextable)

# terms not listed here are dropped from the table
cm <- c(
   "DAC"                   = "DAC",
   "MHI_log"               = "ln(MHI)",
   "incorporated"          = "Incorporated",
   "DAC:incorporated"      = "DAC × Incorporated",
   "MHI_log:incorporated"  = "ln(MHI) × Incorporated",
   "POP_log"               = "ln(Population)",
   "per_latino"            = "% Latino",
   "pages_log"             = "ln(Pages)",
   "(Intercept)"           = "(Intercept)",
   "1|2"                   = "Cutpoint 1 | 2",
   "2|3+"                  = "Cutpoint 2 | 3+",
   "3+|unreachable"        = "Cutpoint 3+ | Unreachable",
   "SD (Intercept GSP_ID)" = "SD (Plan intercept)"
)

# bottom rows: NB dispersion (theta), observations, plans (no AIC / BIC / R2)
extra_rows <- function(mods) {
   plan_ids <- function(m) if (inherits(m, "clmm")) m$model$GSP_ID else m$frame$GSP_ID

   rows <- bind_rows(
      tibble(term = "Observations", !!!map_chr(mods, ~ as.character(nobs(.x)))),
      tibble(term = "Plans", !!!map_chr(mods, ~ as.character(n_distinct(plan_ids(.x))))))

   if (all(map_lgl(mods, ~ inherits(.x, "glmmTMB") && family(.x)$family == "nbinom2"))) {
      theta <- map_chr(mods, ~ sprintf("%.3f", sigma(.x)))
      rows <- bind_rows(tibble(term = "Dispersion (θ)", !!!theta), rows)
   }

   rows
}

# move trailing stars into superscript
sup_stars <- function(ft) {
   dat <- ft$body$dataset
   for (j in ft$col_keys[-1]) for (i in seq_len(nrow(dat))) {
      x <- dat[[j]][i]
      s <- regmatches(x, regexpr("\\*+$", x))
      if (length(s)) {
         ft <- mk_par(ft, i = i, j = j, part = "body",
                      value = as_paragraph(sub("\\*+$", "", x), as_sup(s)))
      }
   }
   ft
}

make_table <- function(mods) {
   # glmmTMB: keep conditional terms only (the NB dispersion is reported as a bottom row)
   is_tmb <- all(map_lgl(mods, ~ inherits(.x, "glmmTMB")))
   rows <- extra_rows(mods)
   if (is_tmb) rows <- add_column(rows, component = "", .after = "term")

   ft <- modelsummary(mods,
                      coef_map = cm,
                      gof_map = NA,
                      add_rows = rows,
                      fmt = 3,
                      stars = c("*" = .05, "**" = .01, "***" = .001),
                      shape = if (is_tmb) term + component ~ model else term + statistic ~ model,
                      group_map = if (is_tmb) c("conditional" = "conditional") else NULL,
                      output = "flextable")
   if (is_tmb) ft <- delete_columns(ft, j = 2)
   ft <- sup_stars(ft)
   ft <- style_table(ft)
   # rule above the bottom rows
   i_gof <- which(ft$body$dataset[[1]] == "SD (Plan intercept)") - 1
   ft <- hline(ft, i = i_gof, border = officer::fp_border(width = 1))
   ft <- align(ft, j = -1, align = "center", part = "all")
   # star note: one cell across the table
   ft <- merge_at(ft, i = 1, j = seq_along(ft$col_keys), part = "footer")
   ft <- align(ft, align = "left", part = "footer")
   autofit(ft)
}

# shared look: top, header, and bottom rules; Times New Roman 10
style_table <- function(ft) {
   ft <- theme_booktabs(ft)
   ft <- font(ft, fontname = "Times New Roman", part = "all")
   ft <- fontsize(ft, size = 10, part = "all")
   autofit(ft)
}
