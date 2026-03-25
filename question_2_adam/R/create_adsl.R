# Programmer : Fanny Gautier
# Date       : 25-MAR-2026
# Program    : /cloud/project/question_2_adam/R/create_adsl.R

# Install necessary packages ----
install.packages("dplyr")
install.packages("pharmaversesdtm")
install.packages("admiral")
install.packages("labelled")

# Load the libraries ----
library(pharmaversesdtm)
library(admiral)
library(dplyr)
library(labelled)

# Load the SDTM data ----
# If SAS data use admiral::convert_blanks_to_na() - Not needed here since rda data
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Population Flag ----
# ITTFL
adsl_0 <- dm %>% 
  derive_var_merged_exist_flag(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ITTFL,
    false_value = "N",
    missing_value = "N",
    condition = (!is.na(ARM))
  )

# Age Grouping ----
# using derive_vars_cat() incl. lookup table
agegr9_lookup <- exprs(
  ~condition,              ~AGEGR9,  ~AGEGR9N,
  !is.na(AGE) & AGE <18, "<18"    ,         1,
  18 <= AGE & AGE <= 50, "18 - 50",         2,
  AGE >50              , ">50"    ,         3,
  is.na(AGE)           , ""       ,        NA
)

adsl_1 <- adsl_0 %>%
  derive_vars_cat(
    definition = agegr9_lookup
  )

#Impute EX Start and End Dates ----
# Get EX character start/End date, convert it to --DTM and impute it
# No need to repeat default arguments
# In RStudio imputed time at "00:00:00" is not visible, but well imputed - EXSTTMF is well populated to "H"
ex_imp <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>% 
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

# Imputed Treatment Start and End Datetime ----
adsl_2 <- adsl_1 %>% 
  # Imputed Treatment Start Datetime
  derive_vars_merged(
    dataset_add = ex_imp,
    # Definition of valid dose - non missing datetime
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    # Create news variables - Keep imputation flag
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    # Order by numerical date and EXSEQ since we want the first available date
    order = exprs(EXSTDTM, EXSEQ),
    # Keep first date per studyid and patient
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
    ) %>% 
  # Imputed Treatment End Datetime - Needed for LSTALVDT derivation
  derive_vars_merged(
    dataset_add = ex_imp,
    # Definition of valid dose - non missing datetime
    filter_add = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    # Create news variables - Keep imputation flag
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    # Keep last date per studyid and patient
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>% 
  # Convert here TRTEDTM to TRTEDT rather than during derive_vars_extreme_event()
  derive_vars_dtm_to_dt(source_vars = exprs(TRTEDTM))
  
# Derive LSTALVDT ----
# Using derive_vars_extreme_event() - one of the most complex function
# Lists of events needed within the function
adsl_3 <- adsl_2 %>% 
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    # (1) last complete date of vital assessment with a valid test result
    # ([VS.VSSTRESN] and [VS.VSSTRESC] not both missing) and datepart of
    # [VS.VSDTC] not missing.
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        # Only non missing and complete date are selected
        condition = !is.na(VSDTC) & nchar(VSDTC)>=10,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC), # Do not impute, default argument
          seq = VSSEQ
        ),
      ),
      # (2) last complete onset date of AEs (datepart of Start Date/Time of
      # Adverse Event [AE.AESTDTC])
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        # Only non missing and complete date are selected
        condition = !is.na(AESTDTC) & nchar(AESTDTC)>=10,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC), # Do not impute, default argument
          seq = AESEQ
        ),
      ),
      # (3) last complete disposition date (datepart of Start Date/Time of
      # Disposition Event [DS.DSSTDTC]).
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        # Only non missing and complete date are selected
        condition = !is.na(DSSTDTC) & nchar(DSSTDTC)>=10,
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC), # Do not impute, default argument
          seq = DSSEQ
        ),
      ),
      # (4) last date of treatment administration where patient received a valid
      # dose (datepart of Datetime of Last Exposure to Treatment [ADSL.TRTEDTM]).
      event(
        dataset_name = "adsl_2",
        # Only non missing are selected
        condition = !is.na(TRTEDT),
        # seq set to 0 as No Sequence Number in ADSL - unique records / No need to sort
        set_values_to = exprs(
          LSTALVDT = TRTEDT,
          seq = 0
          ), 
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl_2 = adsl_2),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, seq, event_nr),
    mode = "last", # Keep last complete date
    new_vars = exprs(LSTALVDT)
  )  

# Final dataset ----
adsl <- adsl_3 %>% 
  # Select Derived Variables as per specs only
  select(STUDYID, USUBJID, AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF, ITTFL, LSTALVDT)

# Apply labels ----
# Label dataset ----
attr(adsl, "label") <- "Subject Level Analysis Dataset"
adsl <- adsl %>% 
  set_variable_labels(
    STUDYID  = "Study Identifier", 
    USUBJID  = "Unique Subject Identifier",
    AGEGR9   = "Age Group 9",
    AGEGR9N  = "Age Group 9 (N)",
    TRTSDTM  = "Datetime of First Exposure to Treatment",
    TRTSTMF  = "Time of First Exposure Imput. Flag",
    ITTFL    = "Intent-To-Treat Population Flag",
    LSTALVDT = "Date Last Known Alive"
  )

# Save ds dataset as RDS
saveRDS(adsl, "/cloud/project/question_2_adam/data/adsl.rds")