library(tidyverse)
library(readxl)

path <- '/Users/krzysztofnowak/Desktop/Kraje.xlsx'

# Setup & Clean File List
files <- list.files(
  path = '/Users/krzysztofnowak/Desktop/',
  pattern = "\\Kraje.xlsx$",
  full.names = TRUE
)

Sheets <- excel_sheets(files)

Scope <- readxl::read_xlsx(files, sheet = 'Scope')$Allow

Sheets |> keep(\(s) s %in% Scope) |> map_dfr(\(s) read_excel(path, sheet = s))

files |>
  set_names('Kolumna') |>
  head(1)
