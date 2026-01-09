library(tidyverse)


Liczby <- c(2, 4, 6, 7, 8)


combn(x = Liczby, m = 3)

combn(x = Liczby, m = 3, FUN = function(x) {
  x[1] * x[2] * x[3]
})


combn(x = Liczby, m = 3, FUN = function(x) {
  reduce(x, `*`)
})

m <- 1000000


options(scipen = 999)
2^log2(m)

?log
