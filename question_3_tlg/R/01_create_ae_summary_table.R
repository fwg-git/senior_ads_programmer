# Programmer : Fanny Gautier
# Date       : 25-MAR-2026
# Program    : /cloud/project/question_3_tlg/R/01_create_ae_summary_table.R

# Install necessary packages ----
install.packages("dplyr")
install.packages("pharmaverseadam")
install.packages("gtsummary")

# Load the libraries ----
library(pharmaverseadam)
library(gtsummary)
library(dplyr)

# Load the ADaM data ----
# If SAS data use admiral::convert_blanks_to_na() - Not needed here since rda data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Filter Safety Population for ADSL
adsl <- adsl %>% 
  filter(SAFFL == "Y")

# Filter AE data ----
adae <- adae %>% 
  filter(SAFFL == "Y" & TRTEMFL == "Y")

tbl <- tbl_hierarchical(
  data = adae,
  variables = c(AESOC, AETERM),
  by = ACTARM,
  id = USUBJID,
  denominator = adsl,
  overall_row = TRUE,
  label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  ) %>%
  # Add Total column, incl. Big N - Total in bold
  add_overall(
    last = TRUE,
    col_label = "**Total**  \nN = {style_number(N)}"
   ) %>% 
  # Sort by descending frequency of the Total column
  sort_hierarchical(
    sort = "descending",
    by = last_col()
  )

# Save output as html
tbl %>% 
  as_gt() %>%
  gt::gtsave("/cloud/project/question_3_tlg/output/ae_summary_table.html")
