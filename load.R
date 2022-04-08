library(tidyverse)
library(docxtractr)

# load PDF that's been converted by Word
breaches_docx <- docxtractr::read_docx("data/source/8555-441-32-b (rotated).pdf edited.docx")

breach_tables_raw <- tibble(breach_table = docx_extract_all_tbls(breaches_docx)) %>%
  mutate(
    table_id = row_number(),
    table_col_n = map_dbl(breach_table, ncol),
    table_row_n = map_dbl(breach_table, nrow)
  ) %>%
  select(table_id, breach_table, everything())

breaches_raw <- breach_tables_raw %>%
  filter(table_col_n == 11) %>% # only response tables
  select(-table_col_n, -table_row_n) %>%
  mutate(breach_table = map(
    breach_table,
    ~ rename(.x,
             date = 1,
             number_individuals = 2,
             summary = 3,
             program = 4,
             individuals_contacted_yes = 5,
             individuals_contacted_no = 6,
             individuals_contacted_date = 7,
             individuals_contacted_how = 8,
             opc_notified_yes = 9,
             opc_notified_no = 10,
             measures_for_individuals = 11
    )
  )) %>%
  unnest(c(breach_table)) %>%
  mutate(
    organization = if_else(
      str_detect(date, fixed("<> ORGANIZATION")),
      date,
      NA_character_
    ),
    organization = str_remove_all(organization, "^<> ORGANIZATION: |<>"),
    organization = str_trim(organization)
  ) %>%
  fill(organization) %>%
  filter(# remove 
    str_detect(date, "\\d"), # require a number of some sort
    ! str_detect(date, fixed("(i) the date")),
    ! str_detect(date, "^Note"),
    ! str_detect(date, "^\\*")
  ) %>%
  group_by(organization) %>%
  mutate(breach_id = paste0(organization, "-", row_number())) %>%
  ungroup()

breaches <- breaches_raw %>%
  mutate(
    number_individuals = case_when( # case-by-case issues
      number_individuals == "Potentially 2.2 Mil" ~ "2,200,000",
      TRUE ~ number_individuals
    ),
    number_individuals = str_remove_all(number_individuals, "\\(Note 2\\)"), # remove an oddity
    number_individuals = str_remove_all(number_individuals, "[^\\d]"), # remove all non-digit values
    number_individuals = as.integer(number_individuals)
  )

