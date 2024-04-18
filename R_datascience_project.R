################################
### Job Salary Data Analysis ###
################################
#                              #
#   Merrimack College          #
#   DSE5002 - R and Python     #
#   Project 01                 #
#   Brendan OConnell           #
#   April 14, 2024             #
#                              #
################################

# -------------------------------------------------------------------
# -------------------------------------------------------------------
### IMPORT PACKAGES ###

library(scales)
library(readr)
library(dplyr)
library(DescTools) # For ISO-3166-1 Country Codes (see https://rdrr.io/cran/DescTools/)
library(ggplot2)
library(tidyr)
library(purrr)

# -------------------------------------------------------------------
# -------------------------------------------------------------------
### COMMON FUNCTIONS ###

# Convert `n` to percentage and round to `decimal`
to_percent <- function(n, decimal=.01){
  label_percent(accuracy = decimal)(n)
  }

# Convert `n` to dollars and round to `decimal`
to_dollar <- function(n, decimal=1){
  label_dollar(accuracy = decimal)(n)
  }

# Find the total count of NA values in `obj`
checkna <- function(obj){
  (sum(is.na(obj)))
  }

# Calculate the number of bins for a histogram
get_numbins <- function(num_entries){
  ceiling(sqrt(num_entries))
  }

# Calculate the bin size for a histogram
get_binsize <- function(num_bins, minmax_diff) (minmax_diff/num_bins)


# -------------------------------------------------------------------
# -------------------------------------------------------------------
##################
## START SCRIPT ##
##################

# Read in the CSV data provided and store as a Dataframe
job_data_df <- read_csv("data/r_project_data.csv", show_col_types = FALSE)

# Verify input data is as expected; check for NA vals
dim(job_data_df)
summary(job_data_df)
print(paste(checkna(job_data_df), 'NA values found.'))

# -------------------------------------------------------------------
# -------------------------------------------------------------------
###################
## DATA CLEANING ##
###################

# Rename 1st column and adjust row ID numbers to start at 1 (instead of 0)
colnames(job_data_df)[1] <- "Row.ID"

if (job_data_df$Row.ID[1] == 0){
  job_data_df$Row.ID <- job_data_df$Row.ID + 1
  }

# CEO is only interested in full-time (FT) -- Subset by FT, remove `employment_type` var
job_data_df <- job_data_df %>% 
  subset(employment_type == 'FT') %>% 
  select(-employment_type)

# Drop variables that aren't needed for analysis.
# Salary & Currency (only need USD salary);
job_data_df <- job_data_df[c(-5, -6)]

# Rename `experience_levels` vals to be more meaningful, then convert to Factor (ordinal var)
curr_levels <- c('EN', 'MI', 'SE', 'EX')
new_levels <- c('Junior', 'Mid', 'Senior', 'Exec')
levels <- tibble(curr_levels, new_levels)

job_data_df <- job_data_df %>%
  left_join(levels, by=c("experience_level" = "curr_levels")) %>% 
  mutate(experience_level = new_levels) %>% 
  mutate(new_levels = NULL)

job_data_df$experience_level <- factor(job_data_df$experience_level ,levels=new_levels)

# Rename `company_size` vars to be more meaningful, then convert to Factor (ordinal var)
curr_sizes <- c('S', 'M', 'L')
new_sizes <- c('Small', 'Medium', 'Large')
sizes <- tibble(curr_sizes, new_sizes)

job_data_df <- job_data_df %>%
  left_join(sizes, by=c("company_size" = "curr_sizes")) %>% 
  mutate(company_size = new_sizes) %>% 
  mutate(new_sizes = NULL)

job_data_df$company_size <- factor(job_data_df$company_size, levels=new_sizes)

# Change `remote_ratio` from a numeric variable to categorical (ordinal)
remote_options <- c("Onsite", "Hybrid", "Remote")

job_data_df <- job_data_df %>% 
  mutate(remote_ratio = case_when(remote_ratio == 0 ~ remote_options[1]
                                  ,remote_ratio == 50 ~ remote_options[2]
                                  ,remote_ratio == 100 ~ remote_options[3]
                                  ,.default = NA
                                  ))
job_data_df <- rename(job_data_df, remote_status=remote_ratio)
job_data_df$remote_status <- factor(job_data_df$remote_status, remote_options)

# Identify & review distinct job titles
jobs <- distinct(job_data_df, job_title)
print(paste(nrow(jobs), 'distinct job titles'))

# Combine similar job titles based on above review
job_rename_df <- read_csv("data/job_rename.csv", show_col_types = FALSE)
print(paste(checkna(job_rename_df), 'NA values found in `job_rename` data.'))
print(paste("Equal length cols in `job_rename` data: "
            ,length(job_rename_df[1]) == length(job_rename_df[2])
            ))
job_data_df <- job_data_df %>%
  left_join(job_rename_df, by = join_by(job_title))

job_data_df <- job_data_df %>% 
  mutate(job_title = if_else(is.na(job_data_df$renamed_title), job_title, renamed_title)
         ,renamed_title = NULL
         )
print(paste(checkna(job_data_df), 'NA values found.'))

# Remove Data Consultants:
# -- Appears to have extreme outliers that impact the analysis. 
# -- Consultants are not viable options for potential leaders of a future team.
#    Their average length at a company is only 2 years.
job_data_df <- job_data_df %>% 
  subset(job_title != "Data Science Consultant")

# Update distinct job titles
jobs <- distinct(job_data_df, job_title) %>% 
  arrange(job_title)
print(paste(nrow(jobs), 'distinct job titles'))

# Get total counts for each job title and join to job dataframe
job_counts <- job_data_df %>% 
  group_by(job_title) %>% 
  summarize(job_count = n()) %>% 
  arrange(-job_count)

job_data_df <- job_data_df %>% 
  left_join(job_counts, by = "job_title")

# Add new column to categorize each job title by a specific "Field"
job_fields <- read_csv("data/job_fields.csv", show_col_types = FALSE)
print(paste(checkna(job_fields), 'NA values found in `job_fields` data.'))
print(paste("Equal length cols in `job_fields` data: "
            ,length(job_fields[1]) == length(job_fields[2])
            ))
job_data_df <- job_data_df %>% 
  left_join(job_fields, by = "job_title")

print(paste(length(unique(job_data_df$job_field)), 'job fields added.'))
print(paste(checkna(job_data_df$job_field), 'NA values found.'))

## ISO 3166-1 COUNTRY CODE DATA
data(d.countries)

# Subset d.countries for country name, a2, a3, and region
country_df <- d.countries[c(1:3,5)]

# Subset by countries that are relevant to the job dataframe entries
employee_loc <- unique(job_data_df$employee_residence)
company_loc <- unique(job_data_df$company_location)
locations <- union(employee_loc, company_loc)

country_df <- country_df %>% 
  subset(country_df$a2 %in% locations)
# Reset row indices
row.names(country_df) <- NULL

# Check for NA values in `country_df`
print(paste(checkna(country_df), "NA value(s) found in `country_df`"))
na_idx <- which(is.na(country_df), arr.ind = TRUE)
print("Indices of NA value(s) to be fixed:")
print(na_idx)

# Fix NA values in `country_df`
country_df$region[is.na(country_df$region)] <- "Europe & Central Asia"
print(paste(checkna(country_df), "NA values found in `country_df`"))

# Join country data on `employee_residence` and convert to 3 letter code
job_data_df <- job_data_df %>% 
  left_join(country_df, by=c("employee_residence" = "a2")) %>% 
  mutate(employee_residence = a3
         ,a2 = NULL
         ,a3 = NULL
         )

# Change company location from 2 letter code to 3 letter code
job_data_df <- job_data_df %>% 
  left_join(country_df[2:3], by=c("company_location" = "a2")) %>% 
  mutate(company_location = a3
         ,a3 = NULL
         )

# Rename the `name` column for country name
job_data_df <- rename(job_data_df, country_name=name)

# Realign columns
job_data_df <- job_data_df %>% 
  relocate(job_field, .before = job_title) %>% 
  relocate(country_name, .after = employee_residence)

# Add domestic/offshore column
job_data_df <- job_data_df %>% 
  mutate(employee_prox = if_else(employee_residence == "USA", "US", "Offshore"))


# -------------------------------------------------------------------
# -------------------------------------------------------------------
###############################
## EXPLORATORY DATA ANALYSIS ##
###############################

# Update default theme setting to center plot titles
theme_update(plot.title = element_text(hjust = 0.5))

# US job data vs Offshore job data
emp_prox <- job_data_df %>% 
  group_by(employee_prox) %>% 
  summarize(pct = to_percent(n()/nrow(job_data_df), 1.0))

# Barplot for comparison of US vs Offshore data entries
emp_prox %>% 
  ggplot(aes(x=employee_prox, y=pct, fill=employee_prox)) +
  geom_bar(stat="identity") +
  labs(x="Employee Residence"
       ,y="Percentage of Data Entries"
       ,title="US vs Offshore Job Data Entries"
       ,fill="Employee Residence"
       ) +
  # Reorder the legend items & x axis
  scale_fill_discrete(limits=c("US","Offshore")) +
  scale_x_discrete(limits=c("US", "Offshore"))

# -------------------------------------------
# Note:  Three part E.D.A.:
#          (1) U.S. Data Analysis
#          (2) Offshore Data Analysis
#          (3) Compare US & Offshore Analysis
# -------------------------------------------


# ---------------------------#
# ---------------------------#
# PART 1: U.S. Data Analysis #
# ---------------------------#
# ---------------------------#

# Subset by US employees
us_df <- job_data_df %>% 
  subset(job_data_df$employee_prox == "US")


# BOXPLOTS for EDA
# -----------------
# Boxplot 1: US Employee Salaries by Year and Experience Level
us_df %>% 
  ggplot(aes(x=factor(work_year)
             ,y=salary_in_usd
             ,color=experience_level
             )) +
  geom_boxplot(outlier.shape=6) +
  labs(x="Work Year",
       y="Annual Salary (US Dollars)"
       ,title="U.S. Employee Salaries by Year and Experience Level"
       ,color="Experience Level"
       ) +
  # Set to Dollars and customize breaks
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     ) +
  # Reorder the legend items so that Exec is on top
  scale_color_discrete(limits=(c("Exec","Senior","Mid","Junior")))


# Boxplot 2: US Senior/Exec Salaries by Year
us_df %>% 
  filter(experience_level == "Senior" | experience_level == "Exec") %>% 
  ggplot(aes(x=factor(work_year), y=salary_in_usd)) +
  geom_boxplot(width=0.25, outlier.shape=6) +
  geom_jitter(width=0.1, size=0.75, color="magenta") +
  labs(x="Work Year",
       y="Annual Salary (US Dollars)"
       ,title="U.S. Senior/Executive Salaries by Year"
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     )


# Aggregate Sr/Exec salaries $100k or less; investigate Job Titles
low_exec_sal <- us_df %>% 
  select(job_title, experience_level, salary_in_usd) %>% 
  filter(experience_level == "Senior" | experience_level == "Exec") %>% 
  filter(salary_in_usd <= 100000)

low_exec_sal_agg <- low_exec_sal$job_title %>% 
  aggregate(list(low_exec_sal$job_title), length)

low_exec_sal_agg

# Subset by Data Science field to narrow job title scope,
# and Senior/Exec levels to focus on top-tier experience
us_datasci <- us_df %>% 
  filter(job_field == "Data Science") %>% 
  filter(experience_level == "Senior" | experience_level == "Exec")


# Boxplot 3: US Senior/Exec Salaries in Data Science field by Year
us_datasci %>% 
  ggplot(aes(x=factor(work_year), y=salary_in_usd)) +
  geom_boxplot(width=0.25, outlier.shape=6) +
  geom_jitter(width=0.1, size=0.75, color="mediumblue") +
  labs(x="Work Year",
       y="Annual Salary (US Dollars)"
       ,title='U.S. Senior/Executive "Data Scientist" Salaries by Year'
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     )


# Boxplot 4: US Senior/Exec Salaries in Data Science field
us_datasci %>% 
  ggplot(aes(x=job_field, y=salary_in_usd)) +
  geom_boxplot(width=0.2, outlier.shape=6,
               outlier.color="firebrick1", outlier.size=3) +
  geom_jitter(width=0.05, size=0.75, color="mediumblue") +
  labs(x="",
       y="Annual Salary (US Dollars)"
       ,title='U.S. Senior/Executive "Data Scientist" Salaries'
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 450000, 50000)
                     ,minor_breaks = seq(0, 450000, 25000)
                     )


# Boxplot 5: US Senior/Exec Salaries in Data Science field by Company Size
us_datasci %>% 
  ggplot(aes(x=factor(company_size)
             ,y=salary_in_usd
             )) +
  geom_boxplot(width=0.25, outlier.shape=6, 
               outlier.color="firebrick1", outlier.size=3) +
  geom_jitter(width=0.1, size=0.75, color="mediumblue") +
  labs(x="Company Size",
       y="Annual Salary (US Dollars)"
       ,title='U.S. Senior/Executive "Data Scientist" Salaries by Company Size'
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     )


# Remove outliers from Large companies with salaries over $300k
us_datasci <- us_datasci %>% 
  filter(salary_in_usd < 300000)



# U.S. DISTRIBUTION PLOTS
# ------------------------
##
# Distribution BOXPLOT
#
us_datasci %>% 
  ggplot(aes(x=job_field
             ,y=salary_in_usd
             )) +
  geom_boxplot(width=0.2, outlier.shape=6, 
               outlier.color="firebrick1", outlier.size=3) +
  geom_jitter(width=0.05, size=1, color="darkorchid4") +
  labs(x="",
       y="Annual Salary (US Dollars)"
       ,title='U.S. Senior/Executive "Data Scientist" Salaries'
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 450000, 50000)
                     ,minor_breaks = seq(0, 450000, 25000)
                     )

# Aggregate US Senior/Exec Data Scientist Salaries
us_agg_ds_snex <- us_datasci %>% 
  group_by(job_field) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Ensure total counts match
us_agg_ds_snex$cnt == nrow(us_datasci)

# Calculate histogram bins
nbins <- get_numbins(us_agg_ds_snex$cnt)
binsize <- get_binsize(nbins,
                       max(us_datasci$salary_in_usd)-min(us_datasci$salary_in_usd)
                       )
##
# HISTOGRAM: Counts
##
# Distribution of salary data for Sr/Exec "Data Science" jobs (outliers removed)
us_datasci %>% 
  ggplot(aes(x=salary_in_usd)) +
  geom_histogram(binwidth=binsize
                 ,color="palegoldenrod"
                 ,fill="cornflowerblue"
                 ) +
  labs(x="Salary (US Dollars)"
       ,y="Counts"
       ,title='Salary Distribution for U.S. Senior/Exec "Data Scientists"'
       ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(breaks = seq(0,20,2)
                     ,minor_breaks = seq(0,20,1)
                     )

##
# HISTOGRAM: Density w/ curved line
##
# Add a smoothed density curve estimate & compare to bell curve for the same histogram
mean_sal <- us_agg_ds_snex$median_sal
sd_sal <- us_agg_ds_snex$mad_sal
# Credit to top answer from StackOverflow for assisting with this code:
# https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
us_datasci %>% 
  ggplot(aes(x=salary_in_usd)) +
  geom_histogram(aes(y=after_stat(density))
                 ,binwidth = binsize
                 ,color="palegoldenrod"
                 ,fill="cornflowerblue"
                 ) +
  # Smoothed Density line estimate
  stat_density(geom = "line", bw=binsize, size=1,  aes(color="density")) +
  # Normal curve
  stat_function(fun = dnorm,
                args=list(mean=mean_sal, sd=sd_sal)
                ,size=1
                ,aes(color="bell")
                ) +
  labs(x="Salary (US Dollars)"
       ,y="Density"
       ,title='Density of Salary for U.S. Senior/Exec "Data Scientists"'
       ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::number
                     # Secondary y-axis label for counts
                     ,sec.axis = sec_axis(
                       transform = ~ . * binsize * nrow(us_datasci)
                       ,name = "Counts"
                       ,breaks=seq(0,20,2)
                       )) +
  scale_color_manual(name=""
                     ,values=c("deeppink","gray15")
                     ,breaks = c("density", "bell")
                     ,labels=c("Smoothed Density Estimate", "Normal Bell Curve")
                     ) +
  theme(legend.position = "bottom")


# Store important distribution data points
us_q <- quantile(us_datasci$salary_in_usd, 
                 probs = c(0.25, 0.5, 0.75), names=F)
us_min <- min(us_datasci$salary_in_usd)
us_max <- max(us_datasci$salary_in_usd)
us_datasci_dist <- data.frame(
  Min=us_min,
  Q1=us_q[1],
  Median=us_q[2],
  Mean=us_agg_ds_snex$mean_sal,
  Q3=us_q[3],
  Max=us_max
  )
#Check median value (should evaluate to TRUE)
us_q[2] == us_agg_ds_snex$median_sal

# Store distribution data in dollar format for data visual purposes
us_datasci_dollar <- data.frame(
  row.names=c("U.S. Employees (USD Salary)"),
  Min=to_dollar(us_min),
  Q1=to_dollar(us_q[1]),
  Median=to_dollar(us_q[2]),
  Mean=to_dollar(us_agg_ds_snex$mean_sal),
  Q3=to_dollar(us_q[3]),
  Max=to_dollar(us_max)
  )

us_datasci_dist
us_datasci_dollar


# U.S. Salary Trends Analysis
# ---------------------------
# Aggregate US data to examine salary trends from 2020 - 2022

# Agg US Salaries by Year
us_agg_year <- us_df %>% 
  group_by(work_year) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Agg US Senior/Exec Salaries by Job Field & Year
us_agg_exec_field_year <- us_df %>% 
  filter(experience_level == "Senior" | experience_level == "Exec") %>% 
  group_by(job_field, work_year) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Agg US Salaries by Year & Job Field
us_agg_year_field <- us_df %>% 
  group_by(work_year, job_field) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Agg US Salaries by Year, Job Field, and Experience Level
us_agg_year_field_exp <- us_df %>% 
  group_by(work_year, experience_level, job_field) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Agg US Salaries by Year and Experience Level
us_agg_year_exp <- us_df %>% 
  group_by(work_year, experience_level) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Note: 
#   Using Median for avg because the aggregated data includes outliers (intentionally)
##
# Line Plot 1: US Median Salary by Year and Job Field
##
us_agg_year_field %>% 
  ggplot(aes(x=work_year, y=median_sal)) +
  geom_line(aes(color=job_field)) +
  labs(x="Work Year"
       ,y="Median Salary (US Dollars)"
       ,color = "Job Field"
       ,title="US Median Salary by Year and Job Field") +
  facet_grid(job_field ~ .
             ,scales="free_y"
             ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(n.breaks = 3
                     ,labels = c("2020", "2021", "2022")
                     ,minor_breaks = NULL
                     ) +
  # Border for each Facet Grid
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

##
# Line Plot 2: US Median Salary by Year and Experience Level
##
us_agg_year_exp %>% 
  ggplot(aes(x=work_year, y=median_sal)) +
  geom_line(aes(color=experience_level)) +
  labs(x="Work Year"
       ,y="Median Salary (US Dollars)"
       ,color = "Experience Level"
       ,title="US Median Salary by Year and Experience Level") +
  facet_grid(experience_level ~ .
             ,scales="free_y"
             # Reverse the layout of the facet grids
             ,as.table=FALSE
             ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(n.breaks = 3
                     ,labels = c("2020", "2021", "2022")
                     ,minor_breaks = NULL
                     ) +
  # Reorder the legend items so that Exec is on top
  scale_color_discrete(limits=(c("Exec","Senior","Mid","Junior"))) +
  # Border for each Facet Grid
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

##
# Line Plot 3: US Median Salary by Year, Job Field, & Experience
##
us_agg_year_field_exp %>% 
  ggplot(aes(x=work_year, y=median_sal)) +
  geom_line(aes(color=experience_level)) +
  labs(x="Work Year"
       ,y="Median Salary (US Dollars)"
       ,color = "Experience Level"
       ,title="US Median Salary by Year, Job Field, & Experience") +
  facet_grid(job_field ~ .
             ,scales="free_y"
             ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(n.breaks = 3
                     ,labels = c("2020", "2021", "2022")
                     ,minor_breaks = NULL
                     ) +
  # Reorder the legend items so that Exec is on top
  scale_color_discrete(limits=(c("Exec","Senior","Mid","Junior"))) +
  # Border for each Facet Grid
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))






# -------------------------------#
# -------------------------------#
# PART 2: Offshore Data Analysis #
# -------------------------------#
# -------------------------------#

# Subset by Offshore employees
offshore_df <- job_data_df %>% 
  subset(job_data_df$employee_prox == "Offshore")


# BOXPLOTS for EDA
# -----------------
# Boxplot 1: Offshore Employee Salaries by Year & Experience
offshore_df %>% 
  ggplot(aes(x=factor(work_year)
             ,y=salary_in_usd
             ,color=experience_level
             )) +
  geom_boxplot(outlier.shape=6) +
  labs(x="Work Year",
       y="Annual Salary (US Dollars)"
       ,title="Offshore Employee Salaries by Year & Experience"
       ,color="Experience Level"
       ) +
  # Set to Dollars and customize breaks
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     ) +
  # Reorder the legend items so that Exec is on top
  scale_color_discrete(limits=(c("Exec","Senior","Mid","Junior")))


# Boxplot 2: Offshore Senior/Executive Salaries by Year
offshore_df %>% 
  filter(experience_level == "Senior" | experience_level == "Exec") %>% 
  ggplot(aes(x=factor(work_year)
             ,y=salary_in_usd
             )) +
  geom_boxplot(width=0.25, outlier.shape=6) +
  geom_jitter(width=0.1, size=0.75, color="magenta") +
  labs(x="Work Year",
       y="Annual Salary (US Dollars)"
       ,title="Offshore Senior/Executive Salaries by Year"
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     )


# Subset by Data Science field to narrow job title scope,
# and Senior/Exec levels to focus on top-tier experience
offshore_datasci <- offshore_df %>% 
  filter(job_field == "Data Science") %>% 
  filter(experience_level == "Senior" | experience_level == "Exec")





# OFFSHORE DISTRIBUTION PLOTS
# ---------------------------
##
# Distribution BOXPLOT
#
offshore_datasci %>% 
  ggplot(aes(x=job_field
             ,y=salary_in_usd
             )) +
  geom_boxplot(width=0.2, outlier.shape=6, outlier.color="red") +
  geom_jitter(width=0.05, size=1, color="forestgreen") +
  labs(x="",
       y="Annual Salary (US Dollars)"
       ,title='Offshore Senior/Exec "Data Scientist" Salaries'
       ) +
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 450000, 50000)
                     ,minor_breaks = seq(0, 450000, 25000)
                     )

# Aggregate Offshore Senior/Exec Data Scientist Salaries
off_agg_ds_snex <- offshore_datasci %>% 
  group_by(job_field) %>% 
  summarize(mean_sal = mean(salary_in_usd, na.rm = T)
            ,median_sal = median(salary_in_usd, na.rm = T)
            ,sd_sal = sd(salary_in_usd, na.rm = T)
            ,mad_sal = mad(salary_in_usd, na.rm = T)
            ,IQR_sal = IQR(salary_in_usd, na.rm = T)
            ,skewness = moments::skewness(salary_in_usd, na.rm = T)
            ,kurtosis = moments::kurtosis(salary_in_usd, na.rm = T)
            ,cnt = n()
            )
# Ensure total counts match
off_agg_ds_snex$cnt == nrow(offshore_datasci)

# Calculate histogram bins
nbins_off <- get_numbins(off_agg_ds_snex$cnt)
binsize_off <- get_binsize(nbins_off,
                           max(offshore_datasci$salary_in_usd)-min(offshore_datasci$salary_in_usd)
                           )
##
# HISTOGRAM: Counts
##
# Distribution of salary data for Sr/Exec "Data Science" jobs
offshore_datasci %>% 
  ggplot(aes(x=salary_in_usd)) +
  geom_histogram(binwidth=binsize_off
                 ,color="seagreen4"
                 ,fill="thistle3"
                 ) +
  labs(x="Salary (US Dollars)"
       ,y="Counts"
       ,title='Salary Distribution for Offshore Senior/Exec "Data Scientists"'
       ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(breaks = seq(0,20,2)
                     ,minor_breaks = seq(0,20,1)
                     )

##
# HISTOGRAM: Density w/ curved line
##
# Add a smoothed density curve estimate & compare to bell curve for the same histogram
mean_off <- off_agg_ds_snex$median_sal
sd_off <- off_agg_ds_snex$mad_sal

# Credit to top answer from StackOverflow for assisting with this code:
# https://stackoverflow.com/questions/6967664/ggplot2-histogram-with-normal-curve
offshore_datasci %>% 
  ggplot(aes(x=salary_in_usd)) +
  geom_histogram(aes(y=after_stat(density))
                 ,binwidth = binsize_off
                 ,color="seagreen4"
                 ,fill="thistle3"
                 ) +
  # Smoothed Density line estimate
  stat_density(geom = "line", bw=binsize_off, size=1,  aes(color="density")) +
  # Normal curve
  stat_function(fun = dnorm,
                args=list(mean = mean_off, sd = sd_off)
                ,size=1
                ,aes(color="bell")
                ) +
  labs(x="Salary (US Dollars)"
       ,y="Density"
       ,title='Salary Distribution for Offshore Sr/Exec "Data Scientists"'
       ) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::number
                     # Secondary y-axis label for counts
                     ,sec.axis = sec_axis(
                       transform = ~ . * binsize_off * nrow(offshore_datasci)
                       ,name = "Counts"
                       ,breaks=seq(0,5,1)
                       )) +
  scale_color_manual(name=""
                     ,values=c("deeppink","gray15")
                     ,breaks = c("density", "bell")
                     ,labels=c("Smoothed Density Estimate", "Normal Bell Curve")
                     ) +
  theme(legend.position = "bottom")

# Store important distribution data points
off_q <- quantile(offshore_datasci$salary_in_usd,
                  probs = c(0.25, 0.5, 0.75), names=F)
off_min <- min(offshore_datasci$salary_in_usd)
off_max <- max(offshore_datasci$salary_in_usd)
off_datasci_dist <- data.frame(
  Min=off_min,
  Q1=off_q[1],
  Median=off_q[2],
  Mean=off_agg_ds_snex$mean_sal,
  Q3=off_q[3],
  Max=off_max
  )

# Store distribution data in dollar format for data visual purposes
off_datasci_dollar <- data.frame(
  row.names=c("Offshore Employees (USD Salary)"),
  Min=to_dollar(off_min),
  Q1=to_dollar(off_q[1]),
  Median=to_dollar(off_q[2]),
  Mean=to_dollar(off_agg_ds_snex$mean_sal),
  Q3=to_dollar(off_q[3]),
  Max=to_dollar(off_max)
  )

# Check median value (should evaluate to TRUE)
off_q[2] == off_agg_ds_snex$median_sal

off_datasci_dist
off_datasci_dollar






# -----------------------------------#
# -----------------------------------#
# PART 3: Compare U.S. with Offshore #
# -----------------------------------#
# -----------------------------------#

# Combine US & Offshore statistics and calculate the differences
compare_df <- rbind(off_datasci_dist, us_datasci_dist)
diffs <- map_dbl(.x=compare_df, .f=diff)

# Convert the differences to dollars
diffs_dollar <- data.frame(
  row.names=c("Difference (USD Salary)"),
  Min=to_dollar(diffs[1]),
  Q1=to_dollar(diffs[2]),
  Median=to_dollar(diffs[3]),
  Mean=to_dollar(diffs[4]),
  Q3=to_dollar(diffs[5]),
  Max=to_dollar(diffs[6])
  )

# Combine dollar forms of US, Offshore, and Differences into a DF for visuals
us_offshore_dollar <- rbind(us_datasci_dollar, off_datasci_dollar, diffs_dollar)







# -------------------------------------------------------------------
# -------------------------------------------------------------------
################################################
## ADDITIONAL ANALYSIS FOR DATA SCIENCE TEAM  ##
################################################

# BOXPLOTS for Data Science Team (EDA)
# ------------------------------------

# Calculate lower & upper whiskers
ylim1 <- boxplot.stats(us_df$salary_in_usd)$stats[c(1, 5)]
coord1 <- coord_cartesian(ylim = ylim1*1)
coord2 <- coord_cartesian(ylim = ylim1*1.15)


# Boxplot 1: US Employee Salaries by Job Field
box1 <- ggplot(us_df, aes(x=job_field
                          ,y=salary_in_usd
                          ,color=job_field
                          )) +
  geom_boxplot(outlier.shape=NA) +
  labs(x="Job Field",
       y="Annual Salary (US Dollars)"
       ,title="U.S. Employee Salaries by Job Field"
       ) +
  geom_jitter(width=0.25, size=1) +
  # Set to Dollars and customize breaks
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     ) +
  theme(legend.position = "none")

# Scale y limits based coord calculations
box1 + coord1



# Boxplot 2: US Employee Salaries by Job Field & Experience Level
box2 <- ggplot(us_df, aes(x=job_field
                          ,y=salary_in_usd
                          ,color=experience_level
                          )) +
  geom_boxplot(outlier.shape=NA) +
  labs(x="Job Field",
       y="Annual Salary (US Dollars)"
       ,title="U.S. Employee Salaries by Job Field & Experience"
       ,color="Experience Level"
       ) +
  # Set to Dollars and customize breaks
  scale_y_continuous(labels = scales::dollar
                     ,breaks = seq(0, 600000, 50000)
                     ,minor_breaks = seq(0, 600000, 25000)
                     ) +
  # Reorder the legend items so that Exec is on top
  scale_color_discrete(limits=(c("Exec","Senior","Mid","Junior")))

# Scale y limits based coord calculations
box2 + coord2
