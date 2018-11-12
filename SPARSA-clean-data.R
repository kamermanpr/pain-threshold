############################################################
#                                                          #
#                   Clean SPARS A data                     #
#                                                          #
############################################################

# Load packages
library(magrittr)
library(tidyverse)
library(readxl)

# Import data
path <- 'data-original/SPARS_A/raw-data-18112016-deidentified.xlsx'
data <- map(.x = 1:19,
            ~read_xlsx(path = path,
                       sheet = .x,
                       col_names = TRUE)) %>%
    map_df(~tbl_df(.))

# Clean data
data %<>%
    # Select required columns
    select(Subject,
           `Trial number`,
           Intensity,
           Rating) %>%
    # Rename columns
    rename(PID = Subject,
           trial_number = `Trial number`,
           intensity = Intensity, # Laser stimulus intensity
           rating = Rating) # SPARS rating

## Fix trial numbering issues (accounting for 'missed' readings):
## ID05
trial_n <- c(1:24, 27:45, 53:66, 79:95)
ID05 <- data %>%
    filter(PID == 'ID05') %>%
    mutate(trial_number = trial_n)

## ID06
trial_n <- c(1:25, 27:51, 53:77, 79:101)
ID06 <- data %>%
    filter(PID == 'ID06') %>%
    mutate(trial_number = trial_n)

## ID12
trial_n <- c(1:23, 27:51, 53:73, 79:100)
ID12 <- data %>%
    filter(PID == 'ID12') %>%
    mutate(trial_number = trial_n)

## ID15
trial_n <- 1:104
ID15 <- data %>%
    filter(PID == 'ID15') %>%
    mutate(trial_number = trial_n)

## Add them back
data %<>%
    filter(PID != 'ID05' & PID != 'ID06' &
               PID != 'ID12' & PID != 'ID15') %>%
    bind_rows(ID05, ID06,
              ID12, ID15)

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
### Block A: "3.00, 2.25, 4.00, 3.25, 2.75, 2.25, 2.75, 4.00, 2.75, 1.00, 2.25,
###           3.00, 1.75, 1.25, 2.25, 3.00, 2.50, 1.75, 2.75, 2.00, 4.00, 1.00,
###           3.25, 2.50, 1.25, 3.00"
### Block B: "3.75, 3.75, 3.25, 2.75, 2.00, 2.25, 1.00, 3.25, 2.00, 1.75, 3.50,
###           2.50, 2.50, 2.50, 1.25, 1.25, 2.25, 2.00, 3.50, 2.00, 2.50, 2.50,
###           2.75, 4.00, 1.75, 3.00"
### Block C: "3.75, 1.50, 3.25, 1.50, 3.00, 2.75, 1.00, 2.25, 1.25, 1.75, 3.75,
###           3.50, 1.25, 1.00, 3.50, 3.75, 4.00, 3.50, 3.00, 3.50, 1.75, 3.25,
###           2.50, 1.25, 1.50, 3.75"
### Block D: "1.50, 3.75, 1.25, 1.75, 3.50, 2.00, 4.00, 2.25, 4.00, 1.50, 1.00,
###           4.00, 3.25, 1.00, 1.50, 3.75, 3.00, 1.00, 2.00, 1.75, 2.00, 3.25,
###           3.50, 1.50, 1.50, 2.75"

#### Complete blocks
blocks_complete <- blocks %>%
    # Remove incomplete blocks
    filter(!PID %in% c('ID05', 'ID06', 'ID12'))

unique_complete <- unique(blocks_complete$stimulus_order)

#### Incomplete blocks
blocks_incomplete <- blocks %>%
    # Remove incomplete blocks
    filter(PID %in% c('ID05', 'ID06', 'ID12'))

unique_incomplete <- unique(blocks_incomplete$stimulus_order)

### Find incomplete blocks that best match complete blocks
#### ID05
adist(x = unique_incomplete[1:4], y = unique_complete, partial = TRUE)
unique_incomplete[1]; unique_complete[4] # A
unique_incomplete[2]; unique_complete[3] # D
unique_incomplete[3]; unique_complete[1] # C
unique_incomplete[4]; unique_complete[2] # B

#### ID06
adist(x = unique_incomplete[5:8], y = unique_complete, partial = TRUE)
unique_incomplete[5]; unique_complete[4] # A
unique_incomplete[6]; unique_complete[3] # D
unique_incomplete[7]; unique_complete[1] # C
unique_incomplete[8]; unique_complete[2] # B

#### ID12
adist(x = unique_incomplete[9:12], y = unique_complete, partial = TRUE)
unique_incomplete[9]; unique_complete[2]  # B
unique_incomplete[10]; unique_complete[3] # D
unique_incomplete[11]; unique_complete[4] # A
unique_incomplete[12]; unique_complete[1] # C

# Merge 'blocks' back into data
data %<>%
  left_join(blocks)

# Classify similar blocks across participants
data %<>%
  mutate(block = case_when(
      str_detect(stimulus_order, pattern = unique_complete[[4]]) ~ 'A',
      str_detect(stimulus_order, pattern = unique_incomplete[[1]]) ~ 'A',
      str_detect(stimulus_order, pattern = unique_incomplete[[5]]) ~ 'A',
      str_detect(stimulus_order, pattern = unique_incomplete[[11]]) ~ 'A',
      str_detect(stimulus_order, pattern = unique_complete[[2]]) ~ 'B',
      str_detect(stimulus_order, pattern = unique_incomplete[[4]]) ~ 'B',
      str_detect(stimulus_order, pattern = unique_incomplete[[8]]) ~ 'B',
      str_detect(stimulus_order, pattern = unique_incomplete[[9]]) ~ 'B',
      str_detect(stimulus_order, pattern = unique_complete[[1]]) ~ 'C',
      str_detect(stimulus_order, pattern = unique_incomplete[[3]]) ~ 'C',
      str_detect(stimulus_order, pattern = unique_incomplete[[7]]) ~ 'C',
      str_detect(stimulus_order, pattern = unique_incomplete[[12]]) ~ 'C',
      str_detect(stimulus_order, pattern = unique_complete[[3]]) ~ 'D',
      str_detect(stimulus_order, pattern = unique_incomplete[[2]]) ~ 'D',
      str_detect(stimulus_order, pattern = unique_incomplete[[6]]) ~ 'D',
      str_detect(stimulus_order, pattern = unique_incomplete[[10]]) ~ 'D'
  ))

## Select required columns
data %<>%
    select(PID, block, block_order, trial_number, intensity, rating)

## Sort PID order
data %<>%
    arrange(PID, block, block_order, trial_number)

# Save outputs
write_rds(x = data,
          path = 'data-cleaned/SPARS_A.rds')

write_csv(x = data,
          path = 'data-cleaned/SPARS_A.csv')

