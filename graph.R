install.packages('container')
library(tidyverse)
library(fastmap)
library(container)

#m <- fastmap()
#m$set("x", 10)
#m$get("x")

Graph = fastmap()

Graph$set("Ty", c("alicja", "bartek", "cecylia"))

search_que <- container::add(0)

library(collections)

graph <- dict(
  ty = c("alicja", "bartek", "cecylia")
)

search_queue <- deque()

ref_add(search_queue, graph[["ty"]])
