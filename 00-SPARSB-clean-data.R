############################################################
#                                                          #
#                    Clean SPARS B data                    #
#                                                          #
############################################################

# Load packages
library(magrittr)
library(tidyverse)

# Import data and add column
## Get a list of files in 'orignal-data' directory
file_names <- list.files(path = "original-data/SPARS_B/",
                         pattern = "*.txt")

## Generate a list of imported dataframes
data <- map(.x = paste0("original-data/SPARS_B/", file_names),
             .f = ~ read_tsv(file = .x))

# Name list items
names(data) <- c("ID01", "ID02", "ID03", "ID04",
                  "ID05", "ID06", "ID07")

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
                       block_number = `block number`,
                       trial_number = `trial number`,
                       SPARS = Rating_given_FEST,
                       NRS = Rating_given_P,
                       SRS = Rating_given_NP))

# Gather ratings
data %<>% map(.data = .x,
              ~ gather(.x,
                       key = scale,
                       value = rating,
                       -block_number, -trial_number, -intensity))

# Add PID column
data %<>% map2(.x = .,
               .y = c("ID01", "ID02", "ID03", "ID04",
                      "ID05", "ID06", "ID07"),
               ~ mutate(.data = .x,
                        PID = .y))

# Correct known issue with np_rt (reaction time) and
# rating_given_np (stimulus intensity rating) for ID01.
# (Data entered in wrong columns or not at all)
data$ID01 %<>%
    mutate(rating = ifelse(scale == 'SRS',
                           yes = NA,
                           no = rating))

# Check for missing data in selected columns
## Create column index
cols <- c('PID', 'block_number', 'trial_number', 'intensity', 'scale')

## Print missing cases
map(.x = data,
    ~ filter(.x[cols], !complete.cases(.x[cols])))

## Remove missing data
data %<>%
    map(.x = .,
        ~ filter(.x, complete.cases(.x[cols])))

# Join the seven datasets into one dataframe
data %<>%
    map_df(~ as_data_frame(.x))

# Recalibrate SRS to a -100 to 0 scale (-100 = no sensation, 0 = pain)
data %<>% mutate(rating = ifelse(scale == 'SRS',
                                 yes = rating - 100,
                                 no = rating))

# Rebase block_number to start at 1 and increase monotically
data %<>% group_by(PID) %>%
    arrange(block_number, trial_number) %>%
    mutate(block_number = dense_rank(block_number))

# Ungroup and order columns
data %<>% ungroup() %>%
    select(PID, block_number, trial_number, scale, intensity, rating)

# Save outputs
write_rds(x = data,
          path = 'cleaned-data/SPARS_B.rds')
write_csv(x = data,
          path = 'cleaned-data/SPARS_B.csv')
