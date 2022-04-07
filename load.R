library(tidyverse)

# fix for old version of Java, ref: https://community.rstudio.com/t/rstudio-crashing-with-tabulizer-need-to-install-the-legacy-java-se-6-runtime/87937/3
Sys.setenv(JAVA_HOME="/usr/local/opt/openjdk@11/libexec/openjdk.jdk/Contents/Home")
library(tabulizer)
options(java.parameters = "-Xmx16000m")

breaches_tables <- extract_tables(
  "data/source/8555-441-32-b (rotated).pdf",
  pages = c(10, 11, 100),
  method = "lattice",
  output = "data.frame"
)


library(pdftools)
library(tidytext)

breach_pages <- pdf_text("data/source/8555-441-32-b (rotated).pdf") %>%
  as_tibble() %>%
  mutate(page = row_number()) %>%
  select(page, text = value)



library(docxtractr)

# load PDF that's been converted by Word
breaches_docx <- docxtractr::read_docx("data/source/8555-441-32-b (rotated).pdf edited.docx")

breaches_tables_raw <- tibble(breach_table = docx_extract_all_tbls(breaches_docx)) %>%
  mutate(
    table_id = row_number(),
    table_col_n = map_dbl(breach_table, ncol),
    table_row_n = map_dbl(breach_table, nrow)
  ) %>%
  select(table_id, breach_table, everything())

breaches_tables <- breaches_tables_raw %>%
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
  )
