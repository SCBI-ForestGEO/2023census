# Generate reports based on checks performed on in coming main census data ####
## this script is run automatically when there is a push

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)
library(data.table)
library(dplyr)
library(sf)
library(curl)
library(ggplot2)

# load data ####

## new census data
old_tree <- fread("raw_data/old_trees/tree_table_0.csv")
recruits_tree <-  fread("raw_data/recruits/tree_table_0.csv")

old_stem <-  fread("raw_data/old_trees/stem_table_1.csv")# this is essentially the same as recruits_stem, so will subset
recruits_stem <- fread("raw_data/recruits/stem_table_1.csv") # this is essentially the same as old_stem, so will subset


tree <- rbind(data.table(table = "old_trees", old_tree), data.table(table = "recruits", recruits_tree))
if( any(duplicated(tree$tag))) stop("there are duplicated tags in tree! double check how to bring in recuits")


old_stem <- old_stem[tag %in% old_tree$tag, ]
recruits_stem <- recruits_stem[tag %in% recruits_tree$tag, ]

recruits_stem$dbh_2018_mm <- NA

stem <- rbind(data.table(table = "old_trees", old_stem), data.table(table = "recruits", recruits_stem[, names(old_stem), with = F]))

cat("New census data loaded") # this is to troubleshoot CI on GitHub actions (see where errors happen)


## main census (will need to change to 4th for the 2028 main census)
mainCensus <-  fread(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

cat("3rd census data loaded") # this is to troubleshoot CI on GitHub actions (see where errors happen)


## quadrat layer
quadrats <- st_read(file.path(here(""),"doc/maps/20m_grid/20m_grid.shp"))
cat("quadrat layer loaded") # this is to troubleshoot CI on GitHub actions (see where errors happen)

## checks
checks <- fread("QAQC_reports//GitHubAction_checks.csv")


## source code to convert coordinates
source("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/R_scripts/SIGEO_plot_grid_UTMcoord.R")

# get data together ####
setdiff(names(tree), names(stem)) # need to add those to stem
setdiff(names(stem), names(tree)) # deal with those "status_2023" "notes_2023"


names(tree) <- gsub("status_currentCensus", "status_2023", names(tree)) # this is because change to status_currentCensus only in tree but not in stem, so reverting for now
names(tree) <- gsub("notes_currentCensus", "notes_2023", names(tree)) # this is because change to notes_currentCensus only in tree but not in stem, so reverting for now


stem <- merge(stem, tree[, c("tag", setdiff(names(tree), names(stem)) ), with = F], by = "tag", all.x = T)


stem <- rbind(tree[, names(stem), with = F], stem) # now stem has all the data (old and recruits, trees and stems)

# only keep data that was censused
stem <- stem[census_status %in% c(1, 2), ] # complete - 1, problem - 2, not initiated - 0


# replace and fill out coordinates and quadrat based on lat lon (x and y)

stem[, c("NAD83_X", "NAD83_Y", "gx", "gy")] <- stem[,lonlat_to_NAD83_and_gxgy(.SD), .SDcols  = c("x", "y")]
stem[, c("quadrat")] <- stem[, gxgy_to_quadrat(.SD, numeric = T), .SDcols = c("gx", "gy")]
stem[, c("lx", "ly")] <- stem[,gxgy_to_lxly(.SD), .SDcols  = c("gx", "gy", "quadrat")]


# minor clean up ####

## convert dbh and hom to numeric
cols <- c("dbh", "hom")
mainCensus[, (cols) := lapply(.SD, as.numeric), .SDcols = cols] # hom "NULL" are converted to NA and that throws a warning that can be ignored

## convert quadrat to character and pad 0
mainCensus[, quadrat := ifelse(nchar(quadrat) == 3, paste0("0", quadrat), quadrat)]
stem[, quadrat := ifelse(nchar(quadrat) == 3, paste0("0", quadrat), quadrat)]
quadrats$PLOT <- ifelse(nchar(quadrats$PLOT) == 3, paste0("0", quadrats$PLOT), quadrats$PLOT)

## change column names so they are not so year dependant THESE LINES OF CODE WILL NEED TO BE EDITED IN 2028
names(stem) <- gsub("2018", "previous", names(stem)) # note that status_2021 is mortality 
names(stem) <- gsub("2023", "current", names(stem))


## convert dbh_current to numeric (note: in 2023, it is recorded in mm)
stem[, dbh_current := as.numeric(dbh_current)]

## convert dbh_previous to numeric (note: make sure to take the mm version)
stem[, dbh_previous := as.numeric(dbh_previous_mm)]

## fill in dbh_if_dead
stem[mortality %in% 1 & grepl("D", status_current), dbh_if_dead := dbh_current]

## fill in mort_status
stem[, mort_status := status_current ]
stem[!is.na(living_status), mort_status := living_status  ]


## fill in new HOM
stem[!is.na(as.numeric(hom_alert)) , hom := as.numeric(hom_alert)]

## fix species

### replace species by sp (the one edited by crew) but need to tolower
stem[sp!=species, .(tag,  StemTag, sp, species, notes_current)]
stem[sp!=species, species := tolower(sp)]

stem[, species := na.omit(species)[1], by = tag] # fill species out with first non NA value.

if(any(is.na(stem$species))) stop("there are species that are NA")



### change species based on notes
stem[grepl("Species is not", notes_current, ignore.case = T), .(tag,  StemTag, sp, species, notes_current)] 

stem[grepl("Species is not", notes_current, ignore.case = T), .(tag,  StemTag, sp, species, notes_current, tolower(regmatches(notes_current, gregexpr("(?<=is not \\w{4} is )\\w{4}|(?<=is not \\w{4}, is )\\w{4}", notes_current, ignore.case = T, perl = T))))] 

tags_to_fix <- stem[grepl("Species is not", notes_current, ignore.case = T), .(tag, old_species = species, species =tolower(regmatches(notes_current, gregexpr("(?<=is not \\w{4} is )\\w{4}|(?<=is not \\w{4}, is )\\w{4}", notes_current, ignore.case = T, perl = T))),notes_current)  ]

stem[tag %in% tags_to_fix$tag, ]$species <- tags_to_fix$species[match(stem[tag %in% tags_to_fix$tag, tag], tags_to_fix$tag)]


### now check if species is fixed by tree
if(any(sapply(tapply(stem$species, stem$tag,unique), function(x) length(na.omit(x)) >1))) stop("there are species inconsistencies other than NA within a tree") # empty means nothing other than NA --> GOOD


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
  
  if(!errorName %in% c("quadratIsNotTheSameInAllStems", "speciesIsNotTheSameInAllStems")) {
    currentTable <- currentTable[eval(str2lang(currentTableFilter)), ]
    } else {
      if(errorName %in% "quadratIsNotTheSameInAllStems") currentTable <- currentTable[,if(uniqueN(quadrat) > 1) .SD else .SD[FALSE, ], by = tag] 
  
  if(errorName %in% "speciesIsNotTheSameInAllStems") currentTable <- currentTable[,if(uniqueN(species) > 1) .SD else .SD[FALSE, ],, by = tag]
  
  }

  
  # select columns
  if(!referenceTableSelect %in% "")  reference <- referenceTable[, eval(str2lang(referenceTableSelect)) ] else reference <- referenceTable
  if(!currentTableSelect %in% "")  current <- currentTable[, eval(str2lang(currentTableSelect)) ] else current <- currentTable
  

  idxError <- eval(str2lang(idxError))##
   if(sum(idxError) > 0) {
     referenceTable$StemTag <- ifelse(referenceTable$StemTag == "Q",1, referenceTable$StemTag)
     referenceTable$StemTag <- as.integer(referenceTable$StemTag)
    allErrors <- dplyr::bind_rows(allErrors, data.table(censusType, errorType, errorName, referenceTable[idxError, ]))
    allErrors$StemTag <- as.integer(allErrors$StemTag)
  }
}




# save reports ------------------------------------------------------------
 
columnsToKeep <- c("censusType", "table", "errorName", 
                   "tag", "StemTag", "quadrat", "sp", 
                   "NAD83_X", "NAD83_Y", "x", "y", "lx", "ly", "dbh_previous", 
                   "hom", "codes_previous", "status_previous", 
                   "status_2022", "comment_2022", 
                   "dbh_current", "status_current", "codes_current", "notes_current",
                   "census_status", "mortality", 
                   "mort_status", "crown_position", "crown_intact", "crown_living", 
                   "fad", "liana_load", 
                   "wounded_main_stem", "rotting_trunk", "canker_swelling_deformity", 
                   "lean_angle", "dead_with_resprout", "dbh_if_dead",
                   "personnel", "date_measured"
                   )


if(sum(allErrors$errorType %in% "error") > 0) {
  fwrite(allErrors[errorType %in% "error", intersect(names(allErrors), columnsToKeep), with = F], 
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

if(!is.null(allErrors)) {
  quadTable <- table(allErrors[, .(quadrat, errorName)])
  quadTable <- data.table(quadrat = rownames(quadTable), as.data.frame.matrix(quadTable))
  
  
  quadSummary <- allErrors[, .(nError = sum(errorType %in% "error"), 
                               nWarnings = sum(errorType %in%  "warning"),
                               nMissingStems = sum(errorName %in% "missedStem")), by = quadrat][order(nError, decreasing = T), ]
  
  
  
  write.csv(quadSummary, file.path(here("QAQC_reports"), "quadErrorSummary.csv"), row.names = F)
  write.csv(quadTable, file.path(here("QAQC_reports"), "quadErrorTable.csv"), row.names = F)
  
  
} else {
  file.remove(file.path(here("QAQC_reports"), "quadErrorSummary.csv"))
  file.remove(file.path(here("QAQC_reports"), "quadErrorTable.csv"))
}



# create list of tag numbers that need replacement see https://github.com/SCBI-ForestGEO/2023census/issues/7 ####

x <- stem[codes_current %in% "RT",]

write.csv(x[, .(tag, StemTag, quadrat, sp, lx, ly, dbh_current , status_current)], file = paste0(here("tags"), "/list_tags_needing_new_tags_", format(Sys.time(), "%Y"), ".csv"), row.names = F)



x <-  stem[codes_current %in% "NN",]

write.csv(x[, .(tag, StemTag, quadrat, sp, lx, ly, dbh_current , status_current)], file = paste0(here("tags"), "/list_tags_needing_nails_", format(Sys.time(), "%Y"), ".csv"), row.names = F)


# give a % completion status ####

percent_completion <- round(sum(paste(mainCensus$tag, mainCensus$StemTag) %in% paste(stem$tag, stem$StemTag))  / nrow(mainCensus) * 100) # % old stem sampled

percent_completion_Mortality <- round(nrow(stem[mortality %in% 1 & !is.na(crown_position),]) / nrow (mainCensus[dbh>100 & status %in% "A", ])* 100)# nrow(stem[mortality %in% 1,]) * 100) # % mortality stem done

n_mortality_remaining <- length(setdiff( mainCensus[dbh>100 & status %in% "A", paste(tag, StemTag)], stem[mortality %in% 1 & !is.na(crown_position), paste(tag, StemTag)]))

old_n_mortalityprogressed <- as.numeric(readLines("QAQC_reports/n_mortalityprogressed.txt"))

n_mortalityprogressed <- nrow(stem[census_status %in% 2 & mortality %in% 1,])

write.table(n_mortalityprogressed, "QAQC_reports/n_mortalityprogressed.txt", row.names = F, col.names = F)


n_mortalityTransitioned <- old_n_mortalityprogressed-n_mortalityprogressed


n_stemRemaining <- sum(!paste(mainCensus$tag, mainCensus$StemTag) %in% paste(stem$tag, stem$StemTag))

n_recruits <- sum(! paste(stem$tag, stem$StemTag) %in% paste(mainCensus$tag, mainCensus$StemTag))
n_bigTrees <- sum(grepl("BT", stem$codes_current))
n_RT <- sum(grepl("RT", stem$codes_current))
n_M <- sum(grepl("\\<M\\>", stem$codes_current))

## dispatch quad to remove the stem tag "Q"
stem$StemTag <- as.integer(ifelse(stem$StemTag == "Q",1, stem$StemTag))

n_StemTag <- table(stem$StemTag[stem$StemTag>1])

dailyRate <- stem[,.(n_stem = .N, median_dbh = median(dbh_current ), including_n_recruits = sum(!tag %in% mainCensus$tag)) , by = cut(as.POSIXct(date_measured, format = "%m/%d/%Y %I:%M:%S %p"), "day")]

png(file.path(here("QAQC_reports"), "DailyRate.png"), width = 8, height = 5, units = "in", res = 300)

print(ggplot(dailyRate) + geom_col(aes(y = n_stem, x = as.Date(cut))) +
  labs(x = "Date", 
       y = "n stem"))

dev.off()

dailyRate[, Date:=as.Date(cut)]
dailyRate <- dailyRate[order(Date), .(Date, n_stem, including_n_recruits)]
write.csv(dailyRate, file.path(here("QAQC_reports"), "DailyRate.csv"), row.names = F)


png(file.path(here("QAQC_reports"), "StemTag_Histogram.png"), width = 5, height = 5, units = "in", res = 300)

barplot(n_StemTag, las = 1, xlab = "StemTag #")
dev.off()

table(n_StemTag)
png(file.path(here("QAQC_reports"), "percent_completion.png"), width = 6, height = 2, units = "in", res = 300)
par(mar = c(0,0,0,0), oma = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,(5:-5)*.2, c(
  paste(prettyNum(percent_completion, big.mark = ","), "% old stem sampled"),
  "",
  paste(prettyNum(percent_completion_Mortality, big.mark = ","), "% old mortality stems finished, ", n_mortality_remaining, "mortality stems to go!"),
# paste(prettyNum(n_mortalityTransitioned, big.mark = ","), "mort stems transitioned from 'in progress' to 'finished"),
"",

  paste(prettyNum(n_recruits, big.mark = ","), "recruits"),
  paste(prettyNum(n_bigTrees, big.mark = ","), "big trees"),
  paste(prettyNum(n_M, big.mark = ","), "Multiple stems"),
  paste(prettyNum(n_RT, big.mark = ","), "needing tags"),
  "",
  paste(prettyNum(n_stemRemaining, big.mark = ","), "stems remaining")
            ))
dev.off()

cat("% completion status done") # this is to troubleshoot CI on GitHub actions (see where errors happen)



# Generate warnings and error image  ####


for(what in c("warning", "error")) {
  
  filename <- file.path(here("QAQC_reports"), paste0(what, "s.png"))
  
  if(!is.null(allErrors)) {
    x <- allErrors[errorType %in% what, ]
    
    if(nrow(x) > 0) all_messages <- paste(paste0(toupper(what), "S!!!\n\n"), paste(checks$errorMessage[match(unique(x$errorName), checks$errorName)], collapse = "\n"), "\n\nCLICK HERE TO GO TO FOLDER") else all_messages = paste0("No ", toupper(what), "S")
    
    
    
    
    if(length(all_messages) == 0)  file.remove(filename)
    
    png(filename, width = 5, height = 0.7 + (0.15*length(unique(unique(x$errorName)))), units = "in", res = 300)
    par(mar = c(0,0,0,0))
    plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
    text(0,0.9, all_messages, col = "red", cex = 0.6, pos = 1)
    # title("warnings!!!", col.main= "red", xpd = NULL, line = -1)
    dev.off()
  } else {
    
    file.remove(filename)
  }
}



# Generate map of censused quadrats ####

if(!is.null(allErrors)) {
  
  if(any(allErrors$errorName %in% "quadratIsNA")) {
    # fill in quadrat with where it should be
    allErrors[errorName %in% "quadratIsNA", quadrat := mainCensus$quadrat[match(allErrors[errorName %in% "quadratIsNA", tag], mainCensus$tag)]]
  }
quadrats_with_error <- unique(allErrors[errorType %in% "error", quadrat])
quadrats_with_warnings <- unique(allErrors[errorType %in% "warning", quadrat])
} else {
  quadrats_with_error = character()
  quadrats_with_warnings = character()
}

quadrats <- quadrats %>%
  mutate(completion_status = case_when(PLOT %in%  intersect(quadrats_with_warnings, quadrats_with_error) ~ "warning & error pending",
                                       PLOT %in%  quadrats_with_error ~ "error pending",
                                       PLOT %in%  quadrats_with_warnings ~ "warning pending",
                                       
                                       PLOT %in%  stem$quadrat ~ "done"))

filename <- file.path(here("QAQC_reports"), "map_of_error_and_warnings.png")


ggplot() + 
  geom_sf(data = quadrats, aes(fill = completion_status)) + 
  scale_fill_manual(values = c("done" = "grey", "warning pending"= "yellow", "error pending" = "orange", "warning & error pending" = "red")) + theme_void()

ggsave(filename, width = 9, height = 8, units = "in", dpi = 300)



# save quadrats that don't have any error ####

stemToSave <- stem[quadrat %in% (quadrats %>% filter(completion_status %in% "done") %>% pull(PLOT)), ]

write.csv(stemToSave, "processed_data/scbi.stem4.csv", row.names = F)

# save mortality in the raw data folder of the mortality repo
if(interactive()) {
  mort <- stemToSave[mortality %in% 1, ]
  mort$status_current <- ifelse( mort$status_current %in% "LI", mort$living_status,  mort$status_current)
  mort$comment_mortality[mort$comment_mortality == "CP"] <- ""
  mort$comment_mortality <- paste(mort$notes_current, mort$comment_mortality, sep = "; ")
  mort$comment_mortality[mort$comment_mortality == "; "] <- ""
  
  write.csv(mort, "../SCBImortality/raw_data/Mortality_Survey_2023.csv", row.names = F)
}