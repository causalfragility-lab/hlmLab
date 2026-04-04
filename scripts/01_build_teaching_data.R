# =============================================================================
# 01_build_teaching_data.R
#
# Purpose : Clean and prepare the ECLS-K:2011 Public Use File for
#           the hlmLab paper worked examples.
#
# Input   : ECLS-K:2011 Public Use File (.csv export)
#           Download free from NCES:
#           https://nces.ed.gov/ecls/kindergarten2011.asp
#
# Outputs : d_cross  -- cross-sectional sample, Fall 2010 (8,492 students
#                       in 2,151 schools)
#           d_clean  -- longitudinal sample, all 9 waves (76,428 obs)
#
# Authors : [ANONYMOUS FOR REVIEW]
# Date    : 2025
# Package : hlmLab 0.1.0
# =============================================================================

library(dplyr)
library(haven)

# -----------------------------------------------------------------------------
# 1. USER: Set path to your ECLS-K:2011 CSV file
# -----------------------------------------------------------------------------
data_path <- "ECLS2011.csv"   # <-- change this to your file location

if (!file.exists(data_path)) {
  stop(
    "Please download the ECLS-K:2011 Public Use File from NCES and place\n",
    "the CSV at: ", data_path, "\n",
    "URL: https://nces.ed.gov/ecls/kindergarten2011.asp"
  )
}

# -----------------------------------------------------------------------------
# 2. Load raw data
# -----------------------------------------------------------------------------
raw <- read.csv(data_path, stringsAsFactors = FALSE)
cat("Raw data dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n")

# -----------------------------------------------------------------------------
# 3. Select and rename variables
#    Adjust ECLS-K variable names below if your version uses different names
# -----------------------------------------------------------------------------
d <- raw %>%
  select(
    id          = X_CHID2011,    # Child ID
    schid       = S2_ID,         # School ID
    TimePoint   = R_ROUND,       # Data collection round (character label)
    math_score  = X2MSCALK5,     # Math IRT scale score (use round-specific var)
    SES         = WKSESL,        # SES composite
    weight      = W1C0,          # Base-year child weight
    sex         = X_CHSEX_R,     # Sex
    race        = X_RACETHP_R,   # Race/ethnicity
    school_type = S2KTYPE,       # School type
    school_location = S2KURBAN,  # Urbanicity
    poverty     = WKPOV_R        # Poverty indicator
  )

# NOTE: Variable names may differ depending on your ECLS-K version and the
# specific round selected. Consult the ECLS-K codebook if variables are not
# found. The key variables for this paper are: id, schid, math score by round,
# SES composite, and sampling weight.

# -----------------------------------------------------------------------------
# 4. Recode time points to readable labels
#    Rounds in ECLS-K:2011: 1=Fall K, 2=Spring K, 3=Fall 1, 4=Spring 1, ...
# -----------------------------------------------------------------------------
round_labels <- c(
  "1" = "Fall 2010",   "2" = "Spring 2011",
  "3" = "Fall 2011",   "4" = "Spring 2012",
  "5" = "Fall 2012",   "6" = "Spring 2013",
  "7" = "Spring 2014", "8" = "Spring 2015",
  "9" = "Spring 2016"
)

d <- d %>%
  mutate(TimePoint = recode(as.character(TimePoint), !!!round_labels))

# -----------------------------------------------------------------------------
# 5. Remove excluded / missing schools (ECLS-K special codes)
# -----------------------------------------------------------------------------
excluded_schids <- c(9998, 9997, 9996, 9995, 9994, 9993, 9100)

d_clean <- d %>%
  filter(!schid %in% excluded_schids) %>%
  filter(weight > 0)                   # Remove zero-weight observations

cat("After exclusions:", nrow(d_clean), "rows\n")

# -----------------------------------------------------------------------------
# 6. Standardize SES and create school-mean SES
# -----------------------------------------------------------------------------
d_clean <- d_clean %>%
  mutate(
    SES_z = as.numeric(scale(SES)),
    schid = factor(schid),
    wave  = as.numeric(factor(TimePoint,
              levels = c("Fall 2010","Spring 2011","Fall 2011","Spring 2012",
                         "Fall 2012","Spring 2013","Spring 2014",
                         "Spring 2015","Spring 2016")))
  )

# -----------------------------------------------------------------------------
# 7. Within-cluster centering and cluster-mean SES (Mundlak specification)
# -----------------------------------------------------------------------------
d_clean <- d_clean %>%
  group_by(schid) %>%
  mutate(
    SES_mean = mean(SES_z, na.rm = TRUE),   # School-mean SES (between)
    SES_c    = SES_z - SES_mean             # Within-school centered SES
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 8. Create cross-sectional sample (Fall 2010 = kindergarten entry)
# -----------------------------------------------------------------------------
d_cross <- d_clean %>%
  filter(TimePoint == "Fall 2010")

# -----------------------------------------------------------------------------
# 9. Verify sample sizes
# -----------------------------------------------------------------------------
cat("\n--- Cross-sectional sample (Fall 2010) ---\n")
cat("Students (N)   :", nrow(d_cross), "\n")
cat("Schools  (J)   :", nlevels(droplevels(d_cross$schid)), "\n")
cat("Avg cluster sz :", round(nrow(d_cross) / nlevels(droplevels(d_cross$schid)), 2), "\n")

cat("\n--- Longitudinal sample (all 9 waves) ---\n")
cat("Obs (N)        :", nrow(d_clean), "\n")
cat("Students       :", length(unique(d_clean$id)), "\n")
cat("Schools        :", nlevels(d_clean$schid), "\n")
cat("Waves          :\n")
print(table(d_clean$TimePoint))

cat("\n--- Math score summary (cross-sectional) ---\n")
print(summary(d_cross$math_score))

cat("\n--- SES summary (cross-sectional) ---\n")
print(summary(d_cross$SES_z))

# -----------------------------------------------------------------------------
# 10. Save cleaned datasets
# -----------------------------------------------------------------------------
saveRDS(d_cross, "data_processed/d_cross.rds")
saveRDS(d_clean, "data_processed/d_clean.rds")

cat("\nDatasets saved to data_processed/\n")
cat("Run 02_run_hlmlab_examples.R next.\n")
