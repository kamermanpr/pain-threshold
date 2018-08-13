############################################################
#                                                          #
#           Clean and tidy SPARS study dataset             #
#                                                          #
############################################################
# Load packages
library(magrittr)
library(tidyverse)

# Import data and add column
## Get a list of files in 'orignal-data' directory
file_names <- list.files(path = "./original-data/SPARS_B/",
                         pattern = "*.txt")

## Generate a list of imported dataframes
files <- map(.x = paste0("./original-data/SPARS_B/", file_names),
             .f = ~ read_tsv(file = .x))

# Name list items
names(files) <- c("FST1601a", "FST1601b", "FST1602", "FST1603",
                  "FST1604", "FST1605", "FST1606")

# Remove superfluous columns
files %<>% map(.x = .,
               ~ select(.data = .x,
                        -version, -timestamp, -`lost msec`, -`free VRAM`,
                        -`trial contents`, -(starts_with("int_")),
                        -Partic, -Block, -TTL_fired, -ITI, -X40))

# Rename select columns
files %<>% map(.x = .,
               ~ rename(.data = .x,
                        intensity = Intensity,
                        scale = Scale,
                        block_number = `block number`,
                        trial_number = `trial number`))

# Add id column
files %<>% map2(.x = .,
                .y = c("FST1601a", "FST1601b", "FST1602", "FST1603",
                       "FST1604", "FST1605", "FST1606"),
                ~ mutate(.data = .x,
                         id = .y))

# Check for missing data in select columns
cols <- c('id', 'block_number', 'trial_number', 'intensity', 'scale')
## Print missing cases
map(.x = files,
    ~ filter(.x[cols], !complete.cases(.x[cols])))
## Remove missing cases
files %<>% map(.x = .,
           ~ filter(.x, complete.cases(.x[cols])))

# Fix data recording issue for FST1601a
# (RT_NP values recorded under Rating_given_NP)
files$FST1601a %<>%
  # Move Rating_given_NT values to RT_NP column
  mutate(RT_NP = ifelse(!is.na(Rating_given_NP),
                        yes = Rating_given_NP,
                        no = NA)) %>%
  # Convert all Rating_given_NP to NA (means no rating data for NRS_NP)
  mutate(Rating_given_NP = NA)

# Join the seven datasets into one dataframe
SPARS_raw <- map_df(.x = files,
                   ~ as_data_frame(.x))

# Generate new columns
## Consolidate raw rating scale values into a single column
SPARS_raw %<>% mutate(rating = ifelse(!is.na(Rating_given_FEST), # FEST == SPARS
                                     yes = Rating_given_FEST,
                                     no = ifelse(!is.na(Rating_given_P),
                                                 yes = Rating_given_P,
                                                 no = ifelse(!is.na(Rating_given_NP),
                                                             yes = Rating_given_NP,
                                                             no = NA))))

## Create columns of SPARS-equivalent values for CNRS_P and NRS_NP
SPARS_raw %<>% mutate(rating_eq = ifelse(scale == 'CNRS_P',
                                        yes = rating / 2,
                                        no = ifelse(scale == 'NRS_NP',
                                                    yes = (rating - 100) / 2,
                                                    no = rating)))

## Create a new scale column with NRS_NP and CNRS_P consolidated in COMP
SPARS_raw %<>% mutate(scale_combined = ifelse(scale == 'SPARS',
                                             yes = scale,
                                             no = 'COMBINED'))

## Recalibrate NRS_NP to a -100 to 0 scale (-100 = no sensation, 0 = pain)
SPARS_raw %<>% mutate(rating_combined = ifelse(scale == 'NRS_NP',
                                              yes = rating - 100,
                                              no = rating))

## Center and scale rating data for SPARS/COMBINED
SPARS_raw %<>% group_by(scale_combined) %>%
  mutate(rating_z_combined = scale(rating_combined))

## Center and scale rating data for SPARS/CNRS_P/NRS_NP
SPARS_raw %<>% group_by(scale) %>%
  mutate(rating_z = scale(rating))

## Consolidate reaction time (RT) for each scale into a single column
SPARS_raw %<>% mutate(RT_msec = ifelse(!is.na(RT_FEST), # RT_FEST == RT_SPARS
                                      yes = RT_FEST,
                                      no = ifelse(!is.na(RT_P),
                                                  yes = RT_P,
                                                  no = ifelse(!is.na(RT_NP),
                                                              yes = RT_NP,
                                                              no = NA))))

## Clean-up columns
SPARS_raw %<>% select(-starts_with("Rating_given_"),
                     -starts_with("eval_"),
                     -RT_FEST,
                     -RT_NP,
                     -RT_P)

# Rebase block_number to start at 1 and increase monotically
SPARS_raw %<>% group_by(id) %>%
  arrange(block_number, trial_number) %>%
  mutate(block_number = dense_rank(block_number))

# Recode intensities on ordinal scale (within participant)
SPARS_raw %<>% group_by(id) %>%
  mutate(intensity_rank = dense_rank(intensity))

# Order columns
SPARS_raw %<>% select(id, block_number, trial_number, intensity, intensity_rank,
                     scale, scale_combined, rating, rating_combined,
                     rating_eq, rating_z, rating_z_combined,
                     RT_msec)

# Remove any grouping
SPARS_raw %<>% ungroup()

# Save outputs
write_rds(x = SPARS_raw,
          path = './data/SPARS_B.rds')
write_csv(x = SPARS_raw,
          path = './data/SPARS_B.csv')
