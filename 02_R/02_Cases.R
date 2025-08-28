library(readxl)
library(httr)
library(glue)
library(dplyr)
library(stringr)
library(janitor)
library(here)
library(readr)
library(stringi)
library(fs)
library(lubridate)

xls_path <- here::here("01_Data", "01_Raw_Data", "RELATORIO_DTB_BRASIL_2024_MUNICIPIOS.xls")

df <- read_excel(xls_path, skip = 6) %>%
  clean_names() %>%
  select(codigo_municipio_completo, nome_municipio, nome_uf) %>%
  rename(code = codigo_municipio_completo, city = nome_municipio, state = nome_uf)

# Dengue
output_dir <- here::here("01_Data", "01_Raw_Data", "api_results", "dengue_cases")
dir.create(output_dir, showWarnings = FALSE)

disease <- "dengue"
format <- "csv"
ew_start <- 53
ew_end <- 24
ey_start <- 2014
ey_end <- 2025

for (i in 1:nrow(df)) {
  code <- df$code[i]
  name_raw <- df$city[i]
  uf_raw <- df$state[i]
  
  name_clean <- stri_trans_general(name_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  uf_clean <- stri_trans_general(uf_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  url <- glue("https://info.dengue.mat.br/api/alertcity?geocode={code}&disease={disease}&format={format}&ew_start={ew_start}&ew_end={ew_end}&ey_start={ey_start}&ey_end={ey_end}")
  
  file_path <- file.path(output_dir, glue("{code}_{name_clean}_{uf_clean}.csv"))
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), file_path)
      cat("Success: ", file_path, "\n")
    } else {
      cat("Failure: ", code, name_clean, "Status code:", status_code(response), "\n")
    }
    Sys.sleep(0.3)
  }, error = function(e) {
    cat("Error: ", code, "-", name_clean, ":", e$message, "\n")
  })
}

# Zika
output_dir <- here::here("01_Data", "01_Raw_Data", "api_results", "zika_cases")
dir.create(output_dir, showWarnings = FALSE)
disease <- "zika"
for (i in 1:nrow(df)) {
  code <- df$code[i]
  name_raw <- df$city[i]
  uf_raw <- df$state[i]
  
  name_clean <- stri_trans_general(name_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  uf_clean <- stri_trans_general(uf_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  url <- glue("https://info.dengue.mat.br/api/alertcity?geocode={code}&disease={disease}&format={format}&ew_start={ew_start}&ew_end={ew_end}&ey_start={ey_start}&ey_end={ey_end}")
  
  file_path <- file.path(output_dir, glue("{code}_{name_clean}_{uf_clean}.csv"))
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), file_path)
      cat("Success: ", file_path, "\n")
    } else {
      cat("Failure: ", code, name_clean, "Status code:", status_code(response), "\n")
    }
    Sys.sleep(0.3)
  }, error = function(e) {
    cat("Error: ", code, "-", name_clean, ":", e$message, "\n")
  })
}

# Chikungunya
output_dir <- here::here("01_Data", "01_Raw_Data", "api_results", "chikungunya_cases")
dir.create(output_dir, showWarnings = FALSE)
disease <- "chikungunya"
for (i in 1:nrow(df)) {
  code <- df$code[i]
  name_raw <- df$city[i]
  uf_raw <- df$state[i]
  
  name_clean <- stri_trans_general(name_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  uf_clean <- stri_trans_general(uf_raw, "Latin-ASCII") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all("[^a-zA-Z0-9_]", "_")
  
  url <- glue("https://info.dengue.mat.br/api/alertcity?geocode={code}&disease={disease}&format={format}&ew_start={ew_start}&ew_end={ew_end}&ey_start={ey_start}&ey_end={ey_end}")
  
  file_path <- file.path(output_dir, glue("{code}_{name_clean}_{uf_clean}.csv"))
  
  tryCatch({
    response <- GET(url)
    if (status_code(response) == 200) {
      writeBin(content(response, "raw"), file_path)
      cat("Success: ", file_path, "\n")
    } else {
      cat("Failure: ", code, name_clean, "Status code:", status_code(response), "\n")
    }
    Sys.sleep(0.3)
  }, error = function(e) {
    cat("Error: ", code, "-", name_clean, ":", e$message, "\n")
  })
}

# Combine (Dengue)
folder <- here::here("01_Data", "01_Raw_Data", "api_results", "dengue_cases")
csv_files <- dir_ls(folder, regexp = "\\.csv$")

combined_df <- lapply(seq_along(csv_files), function(i) {
  f <- csv_files[i]
  file_name <- path_file(f)
  code <- str_split(file_name, "_", simplify = TRUE)[, 1]
  meta <- df %>% filter(code == !!code)
  
  df_data <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character()))
  
  df_data$city <- meta$city[1]
  df_data$state <- meta$state[1]
  df_data$geocode <- code
  df_data$source_file <- file_name
  
  return(df_data)
}) %>% bind_rows()

combined_df <- combined_df %>%
  mutate(
    casos_est = as.numeric(casos_est),
    casos = as.numeric(casos),
    SE = as.integer(SE),
    data_iniSE = as.Date(data_iniSE),
    virus = "dengue",
    State = stri_trans_general(state, "Latin-ASCII"),
    State = tolower(State),
  ) %>%
  filter(data_iniSE >= as.Date("2015-01-01") & data_iniSE <= as.Date("2025-06-07"))

write_csv(combined_df, file.path(folder, "dengue_combined.csv"))

# Combine (Zika)
folder <- here::here("01_Data", "01_Raw_Data", "api_results", "zika_cases")
csv_files <- dir_ls(folder, regexp = "\\.csv$")

combined_df <- lapply(seq_along(csv_files), function(i) {
  f <- csv_files[i]
  file_name <- path_file(f)
  code <- str_split(file_name, "_", simplify = TRUE)[, 1]
  meta <- df %>% filter(code == !!code)
  
  df_data <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character()))
  
  df_data$city <- meta$city[1]
  df_data$state <- meta$state[1]
  df_data$geocode <- code
  df_data$source_file <- file_name
  
  return(df_data)
}) %>% bind_rows()

combined_df <- combined_df %>%
  mutate(
    casos_est = as.numeric(casos_est),
    casos = as.numeric(casos),
    SE = as.integer(SE),
    data_iniSE = as.Date(data_iniSE),
    virus = "zika",
    State = stri_trans_general(state, "Latin-ASCII"),
    State = tolower(State),
  ) %>%
  filter(data_iniSE >= as.Date("2015-01-01") & data_iniSE <= as.Date("2025-06-07"))

write_csv(combined_df, file.path(folder, "zika_combined.csv"))

# Combine (Chikungunya)
folder <- here::here("01_Data", "01_Raw_Data", "api_results", "chikungunya_cases")
csv_files <- dir_ls(folder, regexp = "\\.csv$")

combined_df <- lapply(seq_along(csv_files), function(i) {
  f <- csv_files[i]
  file_name <- path_file(f)
  code <- str_split(file_name, "_", simplify = TRUE)[, 1]
  meta <- df %>% filter(code == !!code)
  
  df_data <- read_csv(f, show_col_types = FALSE, col_types = cols(.default = col_character()))
  
  df_data$city <- meta$city[1]
  df_data$state <- meta$state[1]
  df_data$geocode <- code
  df_data$source_file <- file_name
  
  return(df_data)
}) %>% bind_rows()

combined_df <- combined_df %>%
  mutate(
    casos_est = as.numeric(casos_est),
    casos = as.numeric(casos),
    SE = as.integer(SE),
    data_iniSE = as.Date(data_iniSE),
    virus = "chikungunya",
    State = stri_trans_general(state, "Latin-ASCII"),
    State = tolower(State),
  ) %>%
  filter(data_iniSE >= as.Date("2015-01-01") & data_iniSE <= as.Date("2025-06-07"))

write_csv(combined_df, file.path(folder, "chikungunya_combined.csv"))

# Combine all
dengue_df <- read_csv(here("01_Data", "01_Raw_Data", "api_results", "dengue_cases", "dengue_combined.csv"),
                      show_col_types = FALSE) %>%
  mutate(geocode = as.character(geocode))
zika_df <- read_csv(here("01_Data", "01_Raw_Data", "api_results", "zika_cases", "zika_combined.csv"),
                    show_col_types = FALSE) %>%
  mutate(geocode = as.character(geocode))
chik_df <- read_csv(here("01_Data", "01_Raw_Data", "api_results", "chikungunya_cases", "chikungunya_combined.csv"),
                    show_col_types = FALSE) %>%
  mutate(geocode = as.character(geocode))

all_cases <- bind_rows(dengue_df, zika_df, chik_df)

write_csv(all_cases, here("01_Data", "01_Raw_Data", "brazil_cases.csv"))

all_cases <- all_cases %>%
  select(
    date = data_iniSE,
    EW = SE,
    cases_reported = casos,
    cases_estimated = casos_est,
    pop,
    City = city,
    State,
    Virus = virus
  ) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  select(date, EW, cases_reported, cases_estimated, pop, City, State, Virus
  ) 

write_csv(all_cases, here("01_Data", "02_Clean_Data", "brazil_cases_clean.csv"))

case_summary <- all_cases %>%
  mutate(Year = year(date),
         Month = month(date),
         FU = State) %>%
  group_by(FU, Year, Virus, EW, Month) %>%
  summarise(
    cases_reported = sum(cases_reported, na.rm = TRUE),
    cases_estimated = sum(cases_estimated, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(case_summary, here("01_Data", "02_Clean_Data", "Brazil_case_summary.csv"))

brazil_seq_summary_gisaid <- read_csv(here("01_Data", "02_Clean_Data", "brazil_seq_summary_gisaid.csv"),
                                      show_col_types = FALSE)
weekly_capacity <- full_join(
  brazil_seq_summary_gisaid,
  case_summary,
  by = c("FU", "Year", "Virus", "EW", "Month")
)

weekly_proportion <- weekly_capacity %>%
  mutate(
    sequencing_proportion = ifelse(cases_reported > 0,
                                 (N_sequences / cases_reported) * 1000,
                                 NA)
  )

write_csv(weekly_proportion, here("01_Data", "02_Clean_Data", "Brazil_capacity_summary.csv"))
