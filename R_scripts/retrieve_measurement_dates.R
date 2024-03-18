# retrieve actual measure date

# clear environment ####
rm(list = ls())

# load libraries ####

# load data ####

scbi.stem4 <- read("processed_data/scbi.stem4.csv")
all_commits <- read.csv("raw_data/list_of_all_commits_until_March18_2024.csv")


# lopping through all commits to get tag_id and 
## only looking at trees, will assign same date to all of its stems

sha = "3fdc36bd98b34a853c1f0a531f0664a1ea855b62"
x <- read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/2023census/", sha, "/raw_data/old_trees/tree_table_0.csv" ))
