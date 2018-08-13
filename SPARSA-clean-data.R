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
## Rename select columns
data %<>%
  rename(PID = Subject,
         trial_number = `Trial number`,
         intensity = Intensity, # Laser stimulus
         rating = Rating, # FEST rating
         EDA = `CDA.SCR [muS]`, # Electrodermal reaction
         panas_positive = PA, # Positive and Negative Affect Schedule (PANAS)
         panas_negative = `NA`, # PANAS
         age = Age, # Years
         sex = Gender, # Male = 1, Female = 2
         dass42_anxiety = Anxiety, # Depression, Anxiety and Stress Scales (DASS-42)
         dass42_depression = Depr, # DASS-42
         dass42_stress = Stress, # DASS-42
         pcs_magnification = Magnif, # Pain Catastrophizing Scale (PCS)
         pcs_rumination = Rumin, # PCS
         pcs_helplessness = Helplessn) # PCS

## Add block_order
data %<>%
  mutate(block_order = case_when(
    trial_number <= 26 ~ 1,
    trial_number > 26 & trial_number <= 52 ~ 2,
    trial_number > 52 & trial_number <= 78 ~ 3,
    trial_number > 78 ~ 4
    ))

## Cross tabulate PID and block_number to check crossed effects
xtabs(~ PID + block_order, data = data)

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

## Make intensity class character
data %<>%
  mutate(intensity_char = sprintf('%.2f', intensity))

## Converted FEST to a 0-100 positive scale
data %<>%
  mutate(rating_positive = rating + 50)

## Select required columns
data %<>%
  select(PID, block, block_order, trial_number, intensity, intensity_char,
         rating, rating_positive, EDA, age, sex, panas_positive, panas_negative,
         dass42_depression, dass42_anxiety, dass42_stress, pcs_magnification,
         pcs_rumination, pcs_helplessness)

# Save outputs
write_rds(x = data,
          path = 'data/SPARS_A.rds')
write_csv(x = data,
          path = 'data/SPARS_A.csv')
