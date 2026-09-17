#' MakeComputeGraph.R
#'
#' Orchestrates and visualizes the complete computational dependency graph
#' for the TempleCBE clinical research and vignette architecture.
#' Modelled after the Wolfson study MakeComputeGraph.R pipeline pattern.

suppressPackageStartupMessages({
  library(TempleCBE)
  library(dplyr)
})

make_temple_compute_graph <- function() {
  # 1. Project Pipeline Configuration ----------------------------------------
  pipeline_config(
    study_name = "TempleCBE Biostatistical & Survival Analysis Architecture",
    stage_labels = c(
      "01_EDA"            = "Exploratory Data Analysis & Quality Audit",
      "02_Survival_CV"    = "Penalized Coxnet & Nested Cross-Validation",
      "03_Compute_Graph"  = "Pipeline Dependency & Governance Graph",
      "04_SAS_Validation" = "SAS PROC PHREG Benchmark Validation"
    ),
    stage_colors = list(
      "01_EDA"            = "#DDEBF7",
      "02_Survival_CV"    = "#FCE4D6",
      "03_Compute_Graph"  = "#E8DAEF",
      "04_SAS_Validation" = "#D4EFDF"
    )
  )

  # 2. Raw Data Inputs -------------------------------------------------------
  raw_clinical <- FilePath(
    name = "raw_patient_registry",
    path = "data/raw/patient_registry.csv",
    renders = FALSE,
    stage = "01_EDA",
    artifact_role = "raw_data",
    description = "Raw clinical intake data with missing biomarker measurements"
  )

  sas_benchmark_raw <- FilePath(
    name = "tumor_wide_data",
    path = "data/raw/Tumor.xlsx",
    renders = FALSE,
    stage = "04_SAS_Validation",
    artifact_role = "raw_data",
    description = "45-animal longitudinal papilloma count study (SAS Example 85.7)"
  )

  # 3. Stage 01: EDA & Missingness Analysis ----------------------------------
  cleaned_cohort_rds <- FilePath(
    name = "cleaned_cohort_rds",
    path = "data/derived/cleaned_cohort.rds",
    renders = FALSE,
    stage = "01_EDA",
    artifact_role = "derived_data",
    description = "Audited and normalized clinical cohort"
  )

  eda_report <- create_qmd_renderer(
    name = "01_eda_and_missingness",
    path = "vignettes/eda_and_missingness.Rmd",
    deps = list(raw_clinical),
    file_stage = "01_EDA",
    description = "Missingness auditing, outlier fences, and feature normalization"
  )
  eda_report@output <- c(eda_report@output, list(cleaned_cohort_rds))

  # 4. Stage 02: Penalized Coxnet & Nested Cross-Validation ------------------
  survival_cv_report <- create_qmd_renderer(
    name = "02_nested_survival_cv",
    path = "vignettes/nested_survival_cv.Rmd",
    deps = list(cleaned_cohort_rds),
    file_stage = "02_Survival_CV",
    description = "Subject-grouped nested cross-validation and elastic-net Cox modeling"
  )

  # 5. Stage 04: SAS Validation & Time-Dependent Covariates ------------------
  tumor_cp_rds <- FilePath(
    name = "tumor_counting_process_rds",
    path = "data/derived/Tumor1.rds",
    renders = FALSE,
    stage = "04_SAS_Validation",
    artifact_role = "derived_data",
    description = "Start/stop counting-process records generated via tidy_tmerge_cox"
  )

  sas_report <- create_qmd_renderer(
    name = "04_sas_survival",
    path = "vignettes/sas_survival.Rmd",
    deps = list(sas_benchmark_raw),
    file_stage = "04_SAS_Validation",
    description = "Numerical validation against SAS PROC PHREG and tidy start/stop merging"
  )
  sas_report@output <- c(sas_report@output, list(tumor_cp_rds))

  # 6. Stage 03: Compute Graph & Governance ----------------------------------
  graph_report <- create_qmd_renderer(
    name = "03_compute_graph",
    path = "vignettes/compute_graph.Rmd",
    deps = list(eda_report, survival_cv_report, sas_report),
    file_stage = "03_Compute_Graph",
    description = "Dependency graph auditing, staleness detection, and pipeline governance"
  )

  # Assemble full pipeline list
  list(
    raw_clinical,
    sas_benchmark_raw,
    eda_report,
    cleaned_cohort_rds,
    survival_cv_report,
    sas_report,
    tumor_cp_rds,
    graph_report
  )
}

if (sys.nframe() == 0L) {
  pipeline <- make_temple_compute_graph()
  print(pipeline_summary(pipeline))
}
