library(tidyverse)

Dictionary <- tibble(Words = c('fish', 'vista'))
Inputs <- tibble(User = c("hish"))

Dict_vs_User <- Dictionary |>
  cross_join(Inputs)

Dict_vs_User |>
  mutate(Test = map2(User, Words, CommonPart))

##Common per position
Common_Part <- function(W1, W2) {
  W1 <- str_split_1(W1, "")
  W2 <- str_split_1(W2, "")

  M <- matrix(FALSE, nrow = length(W2), ncol = length(W1))

  for (i in seq_along(W1)) {
    for (j in seq_along(W2)) {
      if (W1[i] == W2[j]) {
        if (i > 1 && j > 1) {
          M[j, i] <- M[j - 1, i - 1] + 1
        } else {
          0
        }
      }
    }
  }

  Answer <- paste(W1[which(M > 0, arr.ind = TRUE)[, 1]], collapse = "")

  return(Answer)
}

Common_Part("fosh", "fort")

Dict_vs_User |>
  mutate(Test = map2_chr(User, Words, Common_Part))
