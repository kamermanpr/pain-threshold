############################################################
#                                                          #
#                  Clean SPARS_A data                      #
#                                                          #
############################################################

# Load packages
library(magrittr)
library(tidyverse)
library(readxl)

# Import data
data <- map(.x = 1:19,
            ~read_xlsx(path = 'original-data/SPARS_A/raw-data-18112016-deidentified.xlsx',
                       sheet = .x,
                       col_names = TRUE)) %>%
  map_df(~tbl_df(.))

# Clean data
data %<>%
    # Select columns
    select(Subject, `Trial number`, Intensity, Rating) %>%
    # Rename columns
    rename(PID = Subject,
           trial_number = `Trial number`,
           intensity = Intensity, # Laser stimulus intensity
           rating = Rating) # SPARS rating

## Add block_order
data %<>%
  mutate(block_order = case_when(
    trial_number <= 26 ~ 1,
    trial_number > 26 & trial_number <= 52 ~ 2,
    trial_number > 52 & trial_number <= 78 ~ 3,
    trial_number > 78 ~ 4
    ))

## Cross tabulate PID and block_number to check crossed effects
xtabs(~ PID + block_order,
      data = data)

## Block type
### Get a collapsed list of intensities to define blocks
blocks <- data %>%
  # Convert stimulus intensity to character
  mutate(intensity_char = sprintf('%.2f', intensity)) %>%
  # Collapse column
  group_by(PID, block_order) %>%
  arrange(trial_number) %>%
  summarise(stimulus_order = paste(intensity_char, collapse = ', ')) %>%
  ungroup()

### Inspect unique blocks
#### Remove incomplete cases
##### Remove ID05 (only three blocks)
##### Remove ID06 (incomplete fourth block)
##### Remove ID12 (incomplete third block)
##### Remove ID15 (only one block)
blocks %<>%
  filter(!PID %in% c('ID05', 'ID06', 'ID12', 'ID15'))

blocks_unique <- unique(blocks$stimulus_order)

length(blocks_unique)

blocks_unique

# Merge 'blocks' back into data
data %<>%
  left_join(blocks)

# EXCLUDE ID15 (only one block with 103 trials)
data %<>%
  filter(PID != 'ID15')

# Classify similar blocks across participants
data %<>%
  mutate(block = case_when(
    str_detect(stimulus_order, pattern = blocks_unique[[1]]) ~ 'A',
    str_detect(stimulus_order, pattern = blocks_unique[[2]]) ~ 'B',
    str_detect(stimulus_order, pattern = blocks_unique[[3]]) ~ 'C',
    str_detect(stimulus_order, pattern = blocks_unique[[4]]) ~ 'D',
    TRUE ~ 'Other'
  ))

## Select required columns
data %<>%
  select(PID, block, block_order, trial_number, intensity, rating)

# Save outputs
write_rds(x = data,
          path = 'data/SPARS_A.rds')
write_csv(x = data,
          path = 'data/SPARS_A.csv')
