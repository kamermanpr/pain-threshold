############################################################
#                                                          #
#             Clean and tidy SPARS_B dataset               #
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
data <- map(.x = paste0("./original-data/SPARS_B/", file_names),
            ~ read_tsv(file = .x))

# Name list items
names(data) <- c("FST1601a", "FST1601b", "FST1602", "FST1603",
                  "FST1604", "FST1605", "FST1606")

# Remove superfluous columns
data %<>% map(.x = .,
              ~ select(.data = .x,
                       -version, -timestamp, -`lost msec`, -`free VRAM`,
                       -`trial contents`, -starts_with("int_"), -Scale,
                       -starts_with("eval_"), -starts_with("RT_"),
                       -Partic, -Block, -TTL_fired, -ITI, -X40))

# Rename select columns
data %<>% map(.x = .,
              ~ rename(.data = .x,
                       intensity = Intensity,
                       block = `block number`,
                       trial_number = `trial number`,
                       SPARS = Rating_given_FEST,
                       NRS = Rating_given_P,
                       SRS = Rating_given_NP))

# Gather ratings
data %<>% map(.data = .x,
              ~ gather(.x,
                       key = scale,
                       value = rating,
                       -block, -trial_number, -intensity))

# Add PID column
data %<>% map2(.x = .,
               .y = c("FST1601a", "FST1601b", "FST1602", "FST1603",
                      "FST1604", "FST1605", "FST1606"),
               ~ mutate(.data = .x,
                        PID = .y))

# Recode SRS ratings as <NA> FST1601a
# (reaction time values 'RT_NP' recorded for SRS 'Rating_given_NP')
data$FST1601a %<>%
    mutate(rating = ifelse(scale == 'SRS',
                           yes = NA,
                           no = rating))

# Print incomplete cases
map(.x = data,
    ~ filter(.x, !complete.cases(.x)))

# Remove incomplete cases
data %<>% map(.x = .,
          ~ filter(.x, complete.cases(.x)))

# Join the datasets into a single dataframe
data %<>%
    map_df(~ as_data_frame(.x))

# Recalibrate SRS to a -100 to 0 scale (-100 = no sensation, 0 = pain)
data %<>% mutate(rating = ifelse(scale == 'SRS',
                                 yes = rating - 100,
                                 no = rating))

# Rebase block_number to start at 1 and increase monotically
data %<>% group_by(PID) %>%
  arrange(block, trial_number) %>%
  mutate(block = dense_rank(block))

# Ungroup and order columns
data %<>% ungroup() %>%
    select(PID, block, trial_number, scale, intensity, rating)

# Save outputs
write_rds(x = data,
          path = './data/SPARS_B.rds')

write_csv(x = data,
          path = './data/SPARS_B.csv')
