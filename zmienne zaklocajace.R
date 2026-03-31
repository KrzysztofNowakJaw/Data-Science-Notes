library(tidyverse)
library(corrr)
set.seed(42)
n <- 30

dzieci <- tibble(
  wiek = sample(0:15, n, replace = TRUE),
  rozmiar_buta = round(wiek * 1.8 + 16 + rnorm(n, 0, 1.5)),
  bieglosc = pmin(10, pmax(1, round(wiek * 0.6 + rnorm(n, 0, 1.2))))
) |>
  mutate(
    grupa_wiekowa = case_when(
      wiek <= 4 ~ "Przedszkolak (0–4)",
      wiek <= 9 ~ "Wczesnoszkolny (5–9)",
      TRUE ~ "Starszy (10–15)"
    ),
    grupa_wiekowa = factor(
      grupa_wiekowa,
      levels = c(
        "Przedszkolak (0–4)",
        "Wczesnoszkolny (5–9)",
        "Starszy (10–15)"
      )
    )
  ) |>
  arrange(wiek)


Zaklocone <- corrr::correlate(x = dzieci$rozmiar_buta, y = dzieci$bieglosc)

dzieci |>
  nest_by(grupa_wiekowa) |>
  mutate(C = corrr::correlate(x = data$rozmiar_buta, y = data$bieglosc))


library(dplyr)
library(tidyr)

dzieci |>
  nest_by(grupa_wiekowa) |>
  summarise(
    korelacja = list(
      corrr::correlate(data, quiet = TRUE)
    ),
    r = cor.test(data$rozmiar_buta, data$bieglosc)$estimate,
    p_value = cor.test(data$rozmiar_buta, data$bieglosc)$p.value,
    n = nrow(data),
    .groups = "drop"
  )
