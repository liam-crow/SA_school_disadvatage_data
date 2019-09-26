library(readxl)
library(dplyr)

clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}

# https://data.sa.gov.au/data/dataset/index-of-disadvantage-by-school/resource/80ee45e1-0a99-4117-acee-c79240528060
# Index of Disadvantage by school 2018

# I like consistent names, also index_of_educational_disadvantage is too long, so I renamed it
school_data <- read.csv("https://data.sa.gov.au/data/dataset/4ab8a539-eab5-48e2-8d88-a599d6114126/resource/80ee45e1-0a99-4117-acee-c79240528060/download/index-of-education-disadvantage-by-school-2018.csv") %>% 
  clean_names() %>% 
  rename(index = index_of_educational_disadvantage,
         type = type_of_schooling)

str(school_data)
# 513 schools, 8 variables

# Let's explore the data a bit

table(school_data$type)

library(lattice)
bwplot(index ~ type, school_data,
       scales=list(x=list(rot=45)))

densityplot(~index | type, school_data)


