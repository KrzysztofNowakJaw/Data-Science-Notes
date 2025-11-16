library(dplyr)

# 1️⃣ Parametry
n_rows <- 10e6 # 10 milionów wierszy
min_val <- 1 # minimalna wartość losowa
max_val <- 1000 # maksymalna wartość losowa

# 2️⃣ Symulacja tabeli
big_table <- tibble(
  Index = 1:n_rows,
  RandomInt = sample(min_val:max_val, n_rows, replace = TRUE)
)

# 3️⃣ Sprawdzenie pierwszych wierszy
head(big_table)

# 4️⃣ Zapis do CSV w folderze projektu
csv_path <- file.path(getwd(), "big_table.csv")
write.csv(big_table, csv_path, row.names = FALSE)

cat("Zapisano plik:", csv_path, "\n")
