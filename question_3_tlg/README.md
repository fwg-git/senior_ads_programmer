# Fanny Gautier interview for Senior ADS Programmer position

## Question 3: TLG - Adverse Events Reporting

### Objective

Create outputs for adverse events summary using the `ADAE` dataset and `{gtsummary}`. This tests the ability of the candidate to create regulatory-compliant clinical reports.

### Task

Input datasets: `pharmaverseadam::adae` and `pharmaverseadam::adsl`

1.  Summary Table using `{gtsummary}` - HINT - [FDA Table 10](https://pharmaverse.github.io/cardinal/quarto/catalog/fda-table_10/) Create a summary table of treatment-emergent adverse events (TEAEs).

-   Treatment-emergent AE records will have `TRTEMFL == "Y"` in `pharmaverseadam::adae`
-   Rows: `AETERM` or `AESOC`
-   Columns: Treatment groups (`ACTARM`)
-   Cell values: Count (n) and percentage (%)
-   Include total column with all subjects
-   Sort by descending frequency Output format: HTML/DOCX/PDF file

2.  Visualizations using `{ggplot2)`

-   Plot 1: AE severity distribution by treatment (bar chart or heatmap). AE Severity is captured in the `AESEV` variable in `pharmaverseadam::adae` dataset. Output format: PNG file

-   Plot 2: Top 10 most frequent AEs (with 95% CI for incidence rates). AEs are captured in the `AETERM` variable in the `pharmaverseadam::adae` dataset.

### Deliverables

-   Script to create summary table: `question_3_tlg/R/01_create_ae_summary_table.R`
-   Script to create visualizations: `question_3_tlg/R/02_create_visualizations.R`
-   Text files/log files as evidence for code running error-free
-   Output files:
    -   ae_summary_table.html (or .docx/.pdf)
    -   Two PNG files (one for each output)

### Repository Structure

-   `R` folder contains the scripts.
-   `output` folder contains the Table and the two Graphs.
-   `log` folder contains the copy of the console saved as `.txt`.
