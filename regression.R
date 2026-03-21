# Dane
df <- tibble::tibble(
  Koszykarz = c(
    "Michael Jordan",
    "Shaquille O'Neal",
    "Kareem Abdul-Jabbar",
    "Russell Westbrook",
    "Moses Malone",
    "Jeremy Lin",
    "Boban Marjanović",
    "Wilt Chamberlain"
  ),
  Syn = c(198, 216, 218, 190, 208, 191, 221, 216),
  Ojciec = c(175, 185, 188, 173, 168, 168, 175, 173)
)

# Model regresji
Model <- lm(Syn ~ Ojciec, data = df)
summary(Model)

# Predykcja dla nowych danych
Nowe <- tibble::tibble(Ojciec = 173)
predict(Model, Nowe)

# ── Wizualizacja ────────────────────────────────────────────────────────────────
library(ggplot2)

# Zakres osi X dla linii regresji i przedziału ufności
x_seq <- data.frame(
  Ojciec = seq(min(df$Ojciec) - 5, max(df$Ojciec) + 5, by = 0.5)
)
ci <- predict(Model, x_seq, interval = "confidence", level = 0.95)
band <- cbind(x_seq, as.data.frame(ci))

# Punkt predykcji
pred_point <- data.frame(
  Ojciec = Nowe$Ojciec,
  Syn = predict(Model, Nowe)
)

ggplot(df, aes(x = Ojciec, y = Syn)) +
  geom_ribbon(
    data = band,
    aes(x = Ojciec, ymin = lwr, ymax = upr),
    inherit.aes = FALSE,
    fill = "#378ADD",
    alpha = 0.12
  ) +
  geom_line(
    data = band,
    aes(x = Ojciec, y = fit),
    inherit.aes = FALSE,
    colour = "#378ADD",
    linewidth = 0.9
  ) +
  geom_point(colour = "#378ADD", size = 3.5) +
  geom_text(
    aes(label = Koszykarz),
    hjust = -0.08,
    vjust = 0.5,
    size = 3,
    colour = "grey40"
  ) +
  geom_point(
    data = pred_point,
    aes(x = Ojciec, y = Syn),
    colour = "#D85A30",
    size = 5,
    shape = 17
  ) +
  geom_label(
    data = pred_point,
    aes(
      x = Ojciec,
      y = Syn,
      label = paste0("Predykcja\n", round(Syn, 1), " cm")
    ),
    hjust = -0.1,
    vjust = 0.5,
    size = 3,
    colour = "#D85A30",
    fill = "white",
    label.size = 0.3
  ) +
  scale_x_continuous(limits = c(163, 200), labels = function(x) {
    paste0(x, " cm")
  }) +
  scale_y_continuous(limits = c(175, 235), labels = function(y) {
    paste0(y, " cm")
  }) +
  labs(
    title = "Regresja: wzrost syna ~ wzrost ojca",
    subtitle = paste0("R² = 0.303 | Syn = 40.66 + 0.9486 × Ojciec"),
    x = "Wzrost ojca (cm)",
    y = "Wzrost syna (cm)",
    caption = "Trójkąt = predykcja dla Ojciec = 173 cm"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey50"),
    plot.caption = element_text(colour = "#D85A30")
  )
