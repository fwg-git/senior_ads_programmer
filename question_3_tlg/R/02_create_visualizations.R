# Programmer : Fanny Gautier
# Date       : 25-MAR-2026
# Program    : /cloud/project/question_3_tlg/R/02_create_visualizations.R

# Install necessary packages ----
install.packages("dplyr")
install.packages("pharmaverseadam")
install.packages("ggplot2")
install.packages("scales")

# Load the libraries ----
library(pharmaverseadam)
library(ggplot2)
library(dplyr)
library(scales)

# Load the ADaM data ----
# If SAS data use admiral::convert_blanks_to_na() - Not needed here since rda data
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

## Filter ADSL data ----
adsl <- adsl %>% 
  filter(SAFFL == "Y")

## Filter AE data ----
adae <- adae %>% 
  filter(SAFFL == "Y")

# Plot 1 ----
## Counts ----
ae_count <- adae %>% 
  # Counts per Severity and Treatment ARM
  group_by(ACTARM, AESEV) %>% 
  count() %>% 
  ungroup() 

## Get automatic max y-axis value ----
max_y <- ae_count %>% 
  group_by(ACTARM) %>% 
  summarise(sum_n = sum(n), .groups = "drop") %>% 
  pull(sum_n) %>% 
  max()

## Build the Bar Plot ----
g1 <- ggplot(ae_count, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_bar(stat = "identity", position = "stack") +
  
  # Title, axis-labels and legend
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  
  # Light Grey Background
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "grey95", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    
    # Tick marks
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.10, "cm"),
  ) +
  
  # Y-axis definition
  scale_y_continuous(
    breaks = seq(0, max_y, by = 100)
  )

## Save plot 1 ----
ggsave(
  filename = file.path("/cloud/project/question_3_tlg/output", "plot1.png"),
  plot = g1,
  width = 8,
  height = 6,
  dpi = 300
)

# Plot 2 ----
## Compute statistics ----
# Compute distinct AETERM per patient
aertem_pat <- adae %>% 
  distinct(USUBJID, AETERM)

# Compute number of events
n_event <- aertem_pat %>% 
  count(AETERM, name = "n_event")

# Compute total number of patients in SAFFL
n_total <- adsl %>%
  summarise(n = n()) %>%
  pull(n)

# Compute 95% CI Clopper-pearson
ci <- n_event %>%
  mutate(
    n_total = n_total,
    prop = n_event / n_total
    ) %>%
  rowwise() %>%
  mutate(
    ci = list(binom.test(n_event, n_total)$conf.int),
    lower = ci[[1]],
    upper = ci[[2]]
  ) %>%
  ungroup() %>% 
  # Select the Top 10
  arrange(desc(prop)) %>% 
  head(10)

## Build the Forest Plot ----
x_min <- floor(min(ci$lower, na.rm = TRUE) * 10) / 10
x_max <- ceiling(max(ci$upper, na.rm = TRUE) * 10) / 10

g2 <- ggplot(ci, aes(x = prop, y = reorder(AETERM, prop))) +
  geom_point(size = 2) +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, orientation = "y") +
  
  # Title and axis-Labels
  labs(
    title = paste0("Top 10 Most Frequent Adverse Events\n n = ", n_total, " subjects; 95% CI Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = ""
  ) +
  
  #theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey95"),
    plot.background = element_rect(fill = "white"),
    axis.ticks = element_line(color = "black"),
  ) +
  
  # Define X-axis
  scale_x_continuous(
    limits = c(x_min, x_max),
    breaks = seq(x_min, x_max, by = 0.1),
    labels = percent_format(accuracy = 1) # Display "xx%"
  )

## Save plot 2 ----
ggsave(
  filename = file.path("/cloud/project/question_3_tlg/output", "plot2.png"),
  plot = g2,
  width = 8,
  height = 6,
  dpi = 300
)
