# SurvSet Benchmark Ingestion Fixture (tests/testthat/helper-survset.R)
# Provides standardized benchmark datasets conforming to the SurvSet schema
# (Drysdale et al. 2022, https://github.com/ErikinBC/SurvSet):
# - pid: patient identifier
# - event: binary event indicator (1 = event, 0 = censored)
# - time: follow-up duration (or tstart, tstop for counting-process data)
# - feature matrix X

load_survset_benchmark <- function(cohort = c("veteran", "lung", "rotterdam", "heart", "colon")) {
  cohort <- match.arg(cohort)

  if (!requireNamespace("survival", quietly = TRUE)) {
    testthat::skip("Package 'survival' is required for SurvSet benchmarks.")
  }

  switch(
    cohort,
    "veteran" = {
      # VA Lung Cancer Study: 137 patients, 6 features
      dat <- survival::veteran
      dat$pid <- seq_len(nrow(dat))
      dat$event <- as.integer(dat$status)
      # Retain original time, plus features
      dat$trt <- factor(dat$trt, levels = c(1, 2), labels = c("standard", "test"))
      dat$prior <- factor(dat$prior, levels = c(0, 10), labels = c("no", "yes"))
      dat$celltype <- factor(dat$celltype)
      dat[, c("pid", "time", "event", "trt", "celltype", "karno", "diagtime", "age", "prior")]
    },
    "lung" = {
      # NCCTG Lung Cancer Study: 228 patients
      dat <- stats::na.omit(survival::lung[, c("time", "status", "age", "sex", "ph.ecog", "ph.karno")])
      dat$pid <- seq_len(nrow(dat))
      dat$event <- as.integer(dat$status == 2) # status 1=censored, 2=dead
      dat$sex <- factor(dat$sex, levels = c(1, 2), labels = c("male", "female"))
      dat[, c("pid", "time", "event", "age", "sex", "ph.ecog", "ph.karno")]
    },
    "rotterdam" = {
      # Rotterdam Breast Cancer Study
      dat <- survival::rotterdam
      dat$pid <- dat$pid
      dat$time <- dat$dtime
      dat$event <- as.integer(dat$death)
      dat$meno <- factor(dat$meno, levels = c(0, 1), labels = c("pre", "post"))
      dat$hormon <- factor(dat$hormon, levels = c(0, 1), labels = c("no", "yes"))
      dat$chemo <- factor(dat$chemo, levels = c(0, 1), labels = c("no", "yes"))
      dat[, c("pid", "time", "event", "age", "meno", "size", "grade", "nodes", "pgr", "er", "hormon", "chemo")]
    },
    "heart" = {
      # Stanford Heart Transplant Study (Counting Process format)
      dat <- survival::heart
      dat$pid <- dat$id
      dat$tstart <- dat$start
      dat$tstop <- dat$stop
      dat$transplant <- factor(dat$transplant, levels = c(0, 1), labels = c("control", "transplanted"))
      dat[, c("pid", "tstart", "tstop", "event", "age", "year", "surgery", "transplant")]
    },
    "colon" = {
      # Colon cancer adjuvant trial: relapse or death
      dat <- stats::na.omit(survival::colon[survival::colon$etype == 2, ]) # death only
      dat$pid <- dat$id
      dat$event <- as.integer(dat$status)
      dat$rx <- factor(dat$rx)
      dat$sex <- factor(dat$sex, levels = c(0, 1), labels = c("female", "male"))
      dat[, c("pid", "time", "event", "rx", "sex", "age", "obstruct", "perfor", "adhere", "nodes")]
    }
  )
}
