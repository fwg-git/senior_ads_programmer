# Programmer : Fanny Gautier
# Date       : 25-MAR-2026
# Program    : /cloud/project/question_1_sdtm/R/01_create_ds_domain.R

# Install necessary packages ----
install.packages("dplyr")
install.packages("pharmaverseraw")
install.packages("sdtm.oak")
install.packages("labelled")

# Load the libraries ----
library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)
library(labelled)

# Read CT file ----
url <- "https://raw.githubusercontent.com/pharmaverse/examples/086d73269499104f08df5b7f72be15b7d59cc871/metadata/sdtm_ct.csv"
sdtm_ct <- read.csv(url)

# Load the raw data ----
# If SAS data use admiral::convert_blanks_to_na()
ds_raw <- pharmaverseraw::ds_raw

# Generate identifier variables ----
# oak_id, raw_source, patient_number
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# Map IT.DSSTDAT to DSSTDTC in ISO8601 format ----
# From MM-DD-YYY to YYYY-MM-DD and Cleaning whitespace
ds_raw <- ds_raw %>%
  mutate(
    DSSTDTC = create_iso8601(
      trimws(IT.DSSTDAT),
      .format = "m-d-y" # MM-DD-YYYY format
    )
  )

# Map Reason for Completion/Discontinuation ----
# assign_no_ct() and assign_ct() are not used due to no corresponding terms
# and derivation coming from various source variables
ds_raw <- ds_raw %>% 
  mutate(
    # If OTHERSP is null then map the value in IT.DSDECOD to DSDECOD 
    # If OTHERSP is not null then map the value in OTHERSP to DSDECOD and also to DSTERM
    DSDECOD = if_else(is.na(OTHERSP), IT.DSDECOD, OTHERSP),
    # If OTHERSP is null then map the value in IT.DSTERM to DSTERM
    DSTERM = if_else(!is.na(OTHERSP), OTHERSP, IT.DSTERM),
    # If IT.DSDECOD = Randomized then map DSCAT = PROTOCOL MILESTONE, else DSCAT = DISPOSITION EVENT
    # If OTHERSP is not null then map DSCAT = OTHER EVENT
    DSCAT = case_when(
      !is.na(IT.DSDECOD) & toupper(IT.DSDECOD) == "RANDOMIZED" ~ "PROTOCOL MILESTONE",
      !is.na(IT.DSDECOD) & toupper(IT.DSDECOD) != "RANDOMIZED" ~ "DISPOSITION EVENT",
      !is.na(OTHERSP) ~ "OTHER EVENT",
      TRUE ~ NA_character_
    )
  )

# assign_ct(
#   raw_dat = ds_raw,
#   raw_var = "IT.DSDECOD",
#   tgt_var = "DSCAT",
#   ct_spec = sdtm_ct,
#   ct_clst = "C74558",
#   id_vars = oak_id_vars()
# ) 

# Date & Time of Collection ----
# Map DSDTC using assign_datetime from DSDTCOL (Date) and DSTMCOL (Time)
ds_raw <-ds_raw %>% 
  left_join(
    assign_datetime(
      tgt_var = "DSDTC",
      raw_dat = ds_raw,
      raw_var = c("DSDTCOL", "DSTMCOL"),
      raw_fmt = c("d-m-y", "H:M"),
      raw_unk = c("UN", "UNK")
      ),
    by = c("oak_id", "raw_source", "patient_number")
    )

# Get DM dates to Compute Study Day ----
dm <- pharmaverseraw::dm_raw %>%
  select(PATNUM, IC_DT) %>% 
  mutate(
    RFSTDTC = create_iso8601(IC_DT, .format = "m/d/y")
  ) %>% 
  select(-IC_DT) %>% 
  filter(!is.na(RFSTDTC))

# Create Usual Identifier Variables ----
ds_raw <- ds_raw %>% 
  mutate(
    STUDYID = STUDY,
    DOMAIN = "DS",
    USUBJID = paste0(STUDY, "-", PATNUM),
    VISIT = INSTANCE,
  ) %>% 
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY",
    merge_key = "PATNUM"
  ) 

# Derive VISITNUM ----
# There is no derivation rule specified
visitnum <- ds_raw %>% 
  select(VISIT, DSSTDY) %>% 
  group_by(VISIT) %>% 
  arrange(VISIT, DSSTDY) %>% 
  slice(1) %>% 
  mutate(
    VISITNUM = as.numeric(gsub("[^0-9.]", "", VISIT)),
    VISITNUM = case_when(
      is.na(VISITNUM) & VISIT != "Baseline" ~ DSSTDY,
      is.na(VISITNUM) & VISIT == "Baseline" ~ 0,
      TRUE ~ VISITNUM
    )
  ) %>% 
  select(-DSSTDY)

# Final dataset ----
ds <- ds_raw %>% 
  left_join(visitnum, by = "VISIT") %>%
  # Add DSSEQ
  arrange(USUBJID, DSSTDY, DSTERM)  %>% 
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDY", "DSTERM")
  ) %>%
  # Select Derived Variables only
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY)

# Apply labels ----
# Label dataset ----
attr(ds, "label") <- "Disposition"
ds <- ds %>% 
  set_variable_labels(
    STUDYID  = "Study Identifier", 
    DOMAIN   = "Domain Abbreviation",
    USUBJID  = "Unique Subject Identifier",
    DSSEQ    = "Sequence Number",
    DSTERM   = "Reported Term for the Disposition Event",
    DSDECOD  = "Standardized Disposition Term",
    DSCAT    = "Category for Disposition Event",
    VISITNUM = "Visit Number",
    VISIT    = "Visit Name",
    DSDTC    = "Date/Time of Collection",
    DSSTDTC  = "Start Date/Time of Disposition Event",
    DSSTDY   = "Study Day of Start of Disposition Event"
    )

# Save ds dataset as RDS
saveRDS(ds, "/cloud/project/question_1_sdtm/data/ds.rds")