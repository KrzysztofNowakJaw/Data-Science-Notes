install.packages('stringdist')
library(stringdist)

# Base labels
base_labels <- c("Label_A", "Label_B", "Label_C")

# Function to introduce typos
introduce_typos <- function(label) {
  # Simple typo variations
  variations <- c(
    label,
    gsub("Label", "Labl", label),  # Missing 'e'
    gsub("A", "4", label),          # Replace 'A' with '4'
    gsub("B", "B_", label),         # Add underscore after 'B'
    gsub("C", "C1", label)          # Replace 'C' with 'C1'
  )
  return(variations)
}

# Generate all variations for each base label
all_labels <- unlist(lapply(base_labels, introduce_typos))
all_labels


uniquemodels <- unique(as.character(all_labels))

distancemodels <- stringdistmatrix(uniquemodels,dat,method = "jw")
rownames(distancemodels) <- uniquemodels

hc <- hclust(as.dist(distancemodels))

plot(hc)

