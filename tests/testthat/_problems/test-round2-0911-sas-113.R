# Extracted from test-round2-0911-sas.R:113

# prequel ----------------------------------------------------------------------
synthetic_recurrent_phreg_lst <- function() {
  q <- recurrent_phreg_quantities(cgd_recurrent_data())
  row_for <- function(model, term, se_type) {
    r <- q[q$model == model & q$term == term, , drop = FALSE]
    if (se_type == "model") {
      se <- r$std_error_model
      chisq <- (r$estimate / se)^2
      p <- r$p_value_model
      sprintf(
        "         %-8s 1 %10.5f %10.5f %10.4f %8s %8.3f",
        term, r$estimate, se, chisq,
        if (p < 1e-4) "<.0001" else sprintf("%.4f", p),
        r$hazard_ratio
      )
    } else {
      se <- r$std_error
      chisq <- (r$estimate / se)^2
      p <- r$p_value
      sprintf(
        "         %-8s 1 %10.5f %10.5f %8.3f %10.4f %8s %8.3f",
        term, r$estimate, se, se / r$std_error_model, chisq,
        if (p < 1e-4) "<.0001" else sprintf("%.4f", p),
        r$hazard_ratio
      )
    }
  }
  fit_block <- function(title, model) {
    c(
      "", "                                       The PHREG Procedure", "",
      paste0("         ", title), "",
      "                               Testing Global Null Hypothesis: BETA=0", "",
      "                       Test                 Chi-Square       DF     Pr > ChiSq", "",
      "                       Likelihood Ratio        23.5243        2         <.0001",
      "                       Wald (COVM)             29.1437        2         <.0001",
      "                       Wald (COVS)             53.9374        2         <.0001", "",
      "                              Analysis of Maximum Likelihood Estimates",
      "                              with Model-Based Variance Estimate", "",
      "                             Parameter      Standard                                  Hazard",
      "         Parameter    DF      Estimate         Error    Chi-Square    Pr > ChiSq       Ratio", "",
      row_for(model, "treat", "model"),
      row_for(model, "sex", "model"),
      row_for(model, "age", "model"), "",
      "                              Analysis of Maximum Likelihood Estimates",
      "                              with Sandwich Variance Estimate", "",
      "                             Parameter      Standard   StdErr                               Hazard",
      "         Parameter    DF      Estimate         Error    Ratio    Chi-Square    Pr > ChiSq       Ratio", "",
      row_for(model, "treat", "robust"),
      row_for(model, "sex", "robust"),
      row_for(model, "age", "robust"), "",
      "         ODS ParameterEstimates data set: printed for the record", "",
      "         Parameter    DF     Estimate    StdErr    ChiSq    ProbChiSq    HazardRatio",
      # A PROC PRINT row looks like a model-based table row; the parser
      # must ignore it (it is not inside an MLE table).
      sprintf(
        "         %-8s 1 %10.5f %10.5f %10.4f %8.4f %8.3f",
        "treat", -1.11911, 0.31018, 13.0175, 0.0003, 0.327
      ), ""
    )
  }
  c(
    "                                           The SAS System", "",
    fit_block("Recurrent PHREG -- Andersen-Gill (total time): COVS(AGGREGATE) COVM", "ag"),
    fit_block("Recurrent PHREG -- PWP gap time: COVS(AGGREGATE) COVM", "pwp_gap")
  )
}

# test -------------------------------------------------------------------------
path <- sas_macro_path("benchmark_recurrent_phreg.sas")
expect_true(file.exists(path))
txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
expect_true(grepl("DRAFT -- THIS PROGRAM HAS NEVER BEEN RUN", txt, fixed = TRUE))
expect_true(grepl("no SAS licence", txt, fixed = TRUE))
