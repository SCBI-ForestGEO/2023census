# Generate reports based on checks performed on in coming main census data ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(readxl)
library(data.table)
library(dplyr)
library(rgdal)

# load data ####

## new census data
tree <- setDT(read_xlsx("raw_data/Field_Maps_test_14_mar.xlsx", sheet = 1))
stem <- setDT(read_xlsx("raw_data/Field_Maps_test_14_mar.xlsx", sheet = 2))


cat("New census data loaded") # this is to troubleshoot CI on GitHub actions (see where errors happen)


## main census (will need to change to 4th for the 2028 main census)
mainCensus <-  fread(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

cat("3rd census data loaded") # this is to troubleshoot CI on GitHub actions (see where errors happen)

## checks

checks <- fread("GitHubAction_checks/GitHubAction_checks.csv")


# minor clean up ####

## convert dbh and hom to numeric
cols <- c("dbh", "hom")
mainCensus[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] # hom "NULL" are converted to NA and that throws a warning that can be ignored

## convert tag stemtag and quadrat to character
cols <- c("tag", "StemTag", "quadrat")

mainCensus[, (cols) := lapply(.SD, as.character), .SDcols = cols]


## bring in quadrat to stem
stem <- merge(stem, tree[, .(tag, quadrat, sp, NAD83_X, NAD83_Y, x, y )], by = "tag", all.x = T)


## change column names so they are not so year dependant THESE LINES OF CODE WILL NEED TO BE EDITED IN 2028
names(stem) <- gsub("2018", "previous", names(stem)) # note that status_2021 is mortality 
names(stem) <- gsub("2023", "current", names(stem))


## fill in dbh_if_dead
stem[mortality %in% 1 & grepl("D", status_current), dbh_if_dead := dbh_current]

## fill in mort_status
stem[, mort_status := status_current ]
stem[!is.na(living_status), mort_status := living_status  ]


# PERFORM CHECKS ------------------------------------------------------
cat("Running main census checks") # this is to troubleshoot CI on GitHub actions (see where errors happen)


allErrors <- NULL

for (i in 1:nrow(checks)) {
  
  # bring all info into environment
  list2env(checks[i, ], .GlobalEnv)
  
  cat(errorDescription, 
      "\n")
  
  # go through the step to find the errors
  referenceTable <- get(referenceTable)
  currentTable <- get(currentTable)
  

  #filter rows
  referenceTable <- referenceTable[eval(str2lang(referenceTableFilter)), ] 
  currentTable <- currentTable[eval(str2lang(currentTableFilter)), ]
  
  # select columns
  if(!referenceTableSelect %in% "")  reference <- referenceTable[, eval(str2lang(referenceTableSelect)) ] else reference <- referenceTable
  if(!currentTableSelect %in% "")  current <- currentTable[, eval(str2lang(currentTableSelect)) ] else current <- currentTable
  

  idxError <- eval(str2lang(idxError))
  
  if(sum(idxError) > 0) {
    allErrors <- dplyr::bind_rows(allErrors, data.table(censusType, errorType, errorName, referenceTable[idxError, ]))
  }
}




# save reports ------------------------------------------------------------

columnsToKeep <- c("censusType", "errorName", "tag", "StemTag", "quadrat", "sp", "NAD83_X", "NAD83_Y", "x", "y", "lx", 
                   "ly", "dbh_previous", "hom", "codes_previous", "status_previous", 
                   "status_2021", "comments_2021", "dbh_current", "status_current", 
                   "codes_current", "notes_current", "census_status", "mortality", 
                   "mort_status", "crown_position", "percent_of_crown_intact", "percent_of_crown_living", 
                   "fad", "liana_load", "wounded_main_stem", "rotting_trunk", "canker_swelling_deformity", 
                   "lean_angle_if_greater_than_15_degrees", "dead_with_resprout", "dbh_if_dead", "CreationDate", 
                   "Creator", "EditDate", "Editor" )



if(sum(allErrors$errorType %in% "error") > 0) {
  fwrite(allErrors[errorType %in% "error", ..columnsToKeep], 
    file = file.path(here("QAQC_reports"), "allErrors.csv"), 
    row.names = F
  )
} else  {
  file.remove(file.path(here("QAQC_reports"), "allErrors.csv"))
  
  warning("need to code to save new mortality census")
}


if(sum(allErrors$errorType %in% "warning") > 0) {
  fwrite(allErrors[errorType %in% "warning", ..columnsToKeep], 
         file = file.path(here("QAQC_reports"), "allWarnings.csv"), 
         row.names = F
  )
} else {
  file.remove(file.path(here("QAQC_reports"), "allWarnings.csv"))
  
}


cat("reports prepared") # this is to troubleshoot CI on GitHub actions (see where errors happen)



# Summary files for each quadrat ####

quadTable <- table(allErrors[, .(quadrat, errorName)])
quadTable <- data.table(quadrat = rownames(quadTable), as.data.frame.matrix(quadTable))


quadSummary <- allErrors[, .(nError = sum(errorType %in% "error"), 
                                 nWarnings = sum(errorType %in%  "warning"),
                                 nMissingStems = sum(errorName %in% "missedStem")), by = quadrat][order(nError, decreasing = T), ]



write.csv(quadSummary, file.path(here("QAQC_reports"), "quadErrorSummary.csv"), row.names = F)
write.csv(quadTable, file.path(here("QAQC_reports"), "quadErrorTable.csv"), row.names = F)




# create list of tag numbers that need replacement see https://github.com/SCBI-ForestGEO/2023census/issues/7 ####

x <- stem[codes_current %in% "RT",]

write.csv(x[, .(tag, StemTag, quadrat, sp, lx, ly, dbh_current , status_current)], file = paste0(here("tags"), "/list_tags_needing_new_tags.csv"), row.names = F)



x <-  stem[codes_current %in% "NN",]

write.csv(x[, .(tag, StemTag, quadrat, sp, lx, ly, dbh_current , status_current)], file = paste0(here("tags"), "/list_tags_needing_nails.csv"), row.names = F)


# give a % completion status ####
percent_completion <- round(sum(mainCensus$quadrat %in% stem$quadrat)  / nrow(mainCensus) * 100)

png(file.path(here("QAQC_reports"), "percent_completion.png"), width = 1, height = 1, units = "in", res = 150)
par(mar = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,0, paste(percent_completion, "%"))
dev.off()
# write.table(percent_completion, file = file.path(here("testthat"), "reports/percent_completion.txt"),  col.names = F, row.names = F)

cat("% completion status done") # this is to troubleshoot CI on GitHub actions (see where errors happen)


# Generate warnings and error image  ####

for(what in c("warning", "error")) {
  
  x <- allErrors[errorType %in% what, ]
  
  if(nrow(x) > 0) all_messages <- paste(paste0(toupper(what), "S!!!\n\n"), paste(checks$errorMessage[match(unique(x$errorName), checks$errorName)], collapse = "\n"), "\n\nCLICK HERE TO GO TO FOLDER") else all_messages = paste0("No ", toupper(what), "S")
  
  
  filename <- file.path(here("QAQC_reports"), paste0(what, "s.png"))
  
  
  if(length(all_messages) == 0)  file.remove(filename)
  
  png(filename, width = 5, height = 0.7 + (0.15*length(unique(unique(x$errorName)))), units = "in", res = 300)
  par(mar = c(0,0,0,0))
  plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
  text(0,0.9, all_messages, col = "red", cex = 0.6, pos = 1)
  # title("warnings!!!", col.main= "red", xpd = NULL, line = -1)
  dev.off()
}
 

# Generate map of censused quadrats ####

quadrats <- rgdal::readOGR(file.path(here(""),"doc/maps/20m_grid/20m_grid.shp"))

quadrats_with_error <- unique(allErrors[errorType %in% "error", quadrat])
quadrats_with_warnings <- unique(allErrors[errorType %in% "warning", quadrat])


filename <- file.path(here("QAQC_reports"), "map_of_error_and_warnings.png")

png(filename, width = 9, height = 8, units = "in", res = 300)
par(mar = c(0,3,0,0))

plot(quadrats)
plot(quadrats[quadrats$PLOT %in%  stem$quadrat,], col = "grey", add = T)
plot(quadrats[quadrats$PLOT %in%  quadrats_with_error, ], col = "orange", add = T)
plot(quadrats[quadrats$PLOT %in%  quadrats_with_warnings, ], col = "yellow", add = T)
plot(quadrats[quadrats$PLOT %in%  intersect(quadrats_with_warnings, quadrats_with_error), ], col = "red", add = T)
legend("bottomleft", fill = c("grey", "yellow", "orange", "red"), legend = c("done", "warning pending", "error pending", "warning & error pending"), bty = "n")

dev.off()

