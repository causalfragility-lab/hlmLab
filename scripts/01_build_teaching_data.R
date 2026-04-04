# =============================================================================
# 01_build_teaching_data.R
#
# Purpose : Clean and prepare the ECLS-K:2011 Public Use File for
#           the hlmLab paper worked examples.
#
# Input   : ECLS-K:2011 Public Use File (SPSS .sav or .csv format)
#           Download free from NCES -- no data use agreement required:
#           https://nces.ed.gov/ecls/kindergarten2011.asp
#
#           Steps to obtain the data:
#           1. Go to the URL above
#           2. Click "Data Files and Documentation"
#           3. Download the Public Use Data File (.sav format)
#           4. Place the file in your R working directory
#           5. Set data_path below to match your filename
#
# Outputs : scripts/data_processed/d_cross.rds
#               Cross-sectional sample, Fall 2010
#               N = 8,492 students in 2,151 schools
#           scripts/data_processed/d_clean.rds
#               Longitudinal sample, all 9 waves
#               N = 76,428 student-wave observations
#
# Authors : [ANONYMOUS FOR REVIEW]
# Date    : 2025
# Package : hlmLab 0.1.0
# =============================================================================

library(dplyr)
library(haven)

# -----------------------------------------------------------------------------
# 1. USER SETTING
#    Place the downloaded ECLS-K:2011 file in your working directory and
#    set the filename below. Accepts both .sav (SPSS) and .csv formats.
# -----------------------------------------------------------------------------
data_path <- "ECLSK2011.sav"   # change if your filename differs

# -----------------------------------------------------------------------------
# 2. Load raw data (auto-detects .sav vs .csv)
# -----------------------------------------------------------------------------
if (!file.exists(data_path)) {
  stop(
    "\nFile not found: ", data_path,
    "\n\nPlease download the ECLS-K:2011 Public Use File from NCES:",
    "\nhttps://nces.ed.gov/ecls/kindergarten2011.asp",
    "\n\nPlace the file in your working directory and update data_path above."
  )
}

ext <- tolower(tools::file_ext(data_path))

if (ext == "sav") {
  raw <- haven::read_sav(data_path)
  cat("Loaded SPSS file:", data_path, "\n")
} else if (ext == "csv") {
  raw <- read.csv(data_path, stringsAsFactors = FALSE)
  cat("Loaded CSV file:", data_path, "\n")
} else {
  stop("Unsupported file type. Please supply a .sav or .csv file.")
}

cat("Raw dimensions:", nrow(raw), "rows x", ncol(raw), "columns\n\n")

# -----------------------------------------------------------------------------
# 3. Select and rename variables
#    Variable names match the ECLS-K:2011 Public Use File codebook.
#    Consult the NCES codebook if any variable is not found in your version.
# -----------------------------------------------------------------------------
d <- raw %>%
  select(
    id              = X_CHID2011,    # Child ID
    schid           = S2_ID,         # School ID
    TimePoint       = R_ROUND,       # Data collection round
    math_score      = X2MSCALK5,     # Mathematics IRT scale score
    SES             = WKSESL,        # SES composite
    weight          = W1C0,          # Base-year child sampling weight
    sex             = X_CHSEX_R,     # Child sex
    race            = X_RACETHP_R,   # Race/ethnicity
    school_type     = S2KTYPE,       # School type
    school_location = S2KURBAN,      # School urbanicity
    poverty         = WKPOV_R        # Poverty indicator
  )

cat("Variables selected.\n")

# -----------------------------------------------------------------------------
# 4. Recode time points to descriptive labels
#    ECLS-K:2011 round codes:
#    1 = Fall 2010 (kindergarten entry)    6 = Spring 2013 (Grade 2)
#    2 = Spring 2011 (kindergarten)        7 = Spring 2014 (Grade 3)
#    3 = Fall 2011 (Grade 1)              8 = Spring 2015 (Grade 4)
#    4 = Spring 2012 (Grade 1)            9 = Spring 2016 (Grade 5)
#    5 = Fall 2012 (Grade 2)
# -----------------------------------------------------------------------------
round_labels <- c(
  "1" = "Fall 2010",   "2" = "Spring 2011",
  "3" = "Fall 2011",   "4" = "Spring 2012",
  "5" = "Fall 2012",   "6" = "Spring 2013",
  "7" = "Spring 2014", "8" = "Spring 2015",
  "9" = "Spring 2016"
)

d <- d %>%
  mutate(TimePoint = dplyr::recode(as.character(TimePoint), !!!round_labels))

cat("Time points in data:\n")
print(table(d$TimePoint))

# -----------------------------------------------------------------------------
# 5. Remove excluded schools and zero-weight observations
#    ECLS-K uses special school ID codes for missing/excluded schools
# -----------------------------------------------------------------------------
excluded_schids <- c(9998, 9997, 9996, 9995, 9994, 9993, 9100)

d_clean <- d %>%
  filter(!schid %in% excluded_schids) %>%
  filter(weight > 0)

cat("\nAfter exclusions:\n")
cat("  Rows    :", nrow(d_clean), "\n")
cat("  Schools :", length(unique(d_clean$schid)), "\n")

# -----------------------------------------------------------------------------
# 6. Standardize SES; create school-mean SES and within-school centered SES
#    (Mundlak specification used in Section 5.3 of the paper)
# -----------------------------------------------------------------------------
d_clean <- d_clean %>%
  mutate(
    SES_z = as.numeric(scale(SES)),
    schid = factor(schid),
    wave  = as.numeric(factor(TimePoint,
              levels = c("Fall 2010", "Spring 2011", "Fall 2011",
                         "Spring 2012", "Fall 2012",  "Spring 2013",
                         "Spring 2014","Spring 2015", "Spring 2016")))
  ) %>%
  group_by(schid) %>%
  mutate(
    SES_mean = mean(SES_z, na.rm = TRUE),   # School-mean SES (between-cluster)
    SES_c    = SES_z - SES_mean             # Within-school centered SES
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 7. Cross-sectional sample: kindergarten entry (Fall 2010)
# -----------------------------------------------------------------------------
d_cross <- d_clean %>%
  filter(TimePoint == "Fall 2010")

# -----------------------------------------------------------------------------
# 8. Verify sample sizes match the paper
# -----------------------------------------------------------------------------
cat("\n--- Cross-sectional sample (Fall 2010) ---\n")
cat("  Students (N)        :", nrow(d_cross), "  [expected: 8,492]\n")
cat("  Schools  (J)        :", nlevels(droplevels(d_cross$schid)),
    "  [expected: 2,151]\n")
cat("  Avg students/school :",
    round(nrow(d_cross) / nlevels(droplevels(d_cross$schid)), 2),
    "  [expected: ~3.95]\n")

cat("\n--- Longitudinal sample (all 9 waves) ---\n")
cat("  Obs (N)             :", nrow(d_clean), "  [expected: 76,428]\n")
cat("  Unique students     :", length(unique(d_clean$id)), "\n")
cat("  Schools             :", nlevels(d_clean$schid), "\n")

cat("\n--- Math score summary (cross-sectional) ---\n")
print(summary(d_cross$math_score))

cat("\n--- SES (standardized) summary (cross-sectional) ---\n")
print(summary(d_cross$SES_z))

# -----------------------------------------------------------------------------
# 9. Save processed datasets
#    Note: data_processed/ is listed in .gitignore and is not tracked by Git.
#    Each user must run this script to generate the processed files locally.
# -----------------------------------------------------------------------------
dir.create("scripts/data_processed", showWarnings = FALSE, recursive = TRUE)

saveRDS(d_cross, "scripts/data_processed/d_cross.rds")
saveRDS(d_clean, "scripts/data_processed/d_clean.rds")

cat("\nDatasets saved to scripts/data_processed/\n")
cat("Next step: source('scripts/02_run_hlmlab_examples.R')\n")
