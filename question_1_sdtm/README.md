# \# Fanny Gautier interview for Senior ADS Programmer position

# 

# \## Question 1: SDTM DS Domain Creation using {sdtm.oak}

# 

# \### Objective

# 

# Create an SDTM Disposition (DS) domain dataset from raw clinical trial data using the {sdtm.oak}.

# 

# \### Task

# 

# Develop an R program (`01\_create\_ds\_domain.R`) to create the DS domain using the below:

# 

# \-   Input raw data: `pharmaverseraw::ds\_raw`

# \-   Study controlled terminology \[file](https://github.com/pharmaverse/examples/blob/main/metadata/sdtm\_ct.csv)

# \-   \[Mock-up](https://github.com/pharmaverse/pharmaverseraw/blob/main/vignettes/articles/aCRFs/Subject\_Disposition\_aCRF.pdf) \*Note: there might be misspellings\*

# 

# \### Expected Result

# 

# \-   An error-free program with good documentation that will create the DS domain with the following variables: `STUDYID`, `DOMAIN`, `USUBJID`, `DSSEQ`, `DSTERM`, `DSDECOD`, `DSCAT`, `VISITNUM`, `VISIT`, `DSDTC`, `DSSTDTC`, `DSSTDY`.

# \-   Resulting SDTM dataset in any format.

# \-   A text file/log file as evidence for code running error-free (Console saved into a txt file).

# 

# \### Repository Structure

# 

# \-   `R` folder contains the scripts.

# \-   `data` folder contains the resulting and final dataset.

# \-   `log` folder contains the copy of the console saved as `.txt`.

# 

