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

# get data together ####
setdiff(names(tree), names(stem)) # need to add those to stem
setdiff(names(stem), names(tree)) # deal with those "status_2023" "notes_2023"


names(tree) <- gsub("status_currentCensus", "status_2023", names(tree)) # this is because change to status_currentCensus only in tree but not in stem, so reverting for now
names(tree) <- gsub("notes_currentCensus", "notes_2023", names(tree)) # this is because change to notes_currentCensus only in tree but not in stem, so reverting for now


stem <- merge(stem, tree[, c("tag", setdiff(names(tree), names(stem)) ), with = F], by = "tag", all.x = T)


stem <- rbind(tree[, names(stem), with = F], stem)

# only keep data that was censused
stem <- stem[census_status %in% c(1, 2), ] # complete - 1, problem - 2, not initiated - 0



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


# 
# 
# speed <- stem[,.(n_stem = .N, median_dbh = median(dbh_current ), time1 = head(sort(date), 1), time2 = tail(sort(date),1), n_recruits = sum(!tag %in% mainCensus$tag)) , by = quadrat]
# 
# 
# speed[, mean_duration := difftime(time2, time1, units = "days")]
# 
# all <- mainCensus[,.(n_stem = .N, median_dbh = median(dbh, na.rm = T)) , by = quadrat]
# 
# 
# ggplot(all, aes(x = n_stem, y = median_dbh)) + geom_point( alpha = 0.1) +
#   geom_point(data = speed, aes(color = log(as.numeric(mean_duration)), size = n_recruits)) +
#   scale_x_continuous(trans='log10') +
#   scale_y_continuous(trans='log10') +
#   labs( x = "n stem (log)",
#         y = "meadian dbh (log)",
#         color = "average duration (days)",
#         size = "n recruits") +
#   theme_classic()
#   
# 
# 
# 
# m <- lm(as.numeric(mean_duration) ~ n_stem + median_dbh*n_recruits + time1, data = speed)
# mr <- glm(n_recruits ~ n_stem + median_dbh, data = speed, family = poisson)
# 
# all$n_recruits <- exp(predict(mr, all))
# all$time1 <- speed$time1[match(all$quadrat, speed$quadrat)]
# all[is.na(time1), time1 := Sys.time()]
# 
# all$predicted_duration <- predict(m, all)
# all$actual_duration <- speed$mean_duration[match(all$quadrat, speed$quadrat)]
# 
# all[ , ObsMinusExp := actual_duration - predicted_duration ]
# 
# plot(actual_duration ~ predicted_duration, data = all)
# abline(0, 1)
# 
# ggplot(all[!is.na(actual_duration)], aes(x = n_stem, y = median_dbh, color = as.numeric(ObsMinusExp), size = n_recruits)) + geom_point() +
#   scale_x_continuous(trans='log10') +
#   scale_y_continuous(trans='log10') +
#   scale_colour_gradient2 ()+
#   labs( x = "n stem (log)",
#         y = "meadian dbh (log)",
#         color = "observed minus expected duration",
#         size = "expected n recruits") 
# 
# 
# 
# percent_time_alapsed <- sum(all[!is.na(actual_duration), predicted_duration])*100/sum(all$predicted_duration)
# percent_time_remaing <- sum(all[is.na(actual_duration), predicted_duration])*100/sum(all$predicted_duration)
# 
# 
# current_time_alapsed <- difftime(Sys.time(), min(stem$date, na.rm = T)) # current time allapsed
# 
# remaining_time <- current_time_alapsed * percent_time_remaing / percent_time_alapsed
# 
# Sys.time() + remaining_time
# 
# ggplot(speed, aes(x = time1, y = mean_duration)) + geom_point()
# 
# 
# ggplot(dailyRate, aes(x = as.Date(cut), y = n_stem, size = n_recruits)) + geom_point()
# ggplot(dailyRate, aes(x = as.Date(cut), y = median_dbh )) + geom_point()
# ggplot(dailyRate, aes(x = as.Date(cut), y = n_recruits )) + geom_point()
# ggplot(dailyRate, aes(x = as.Date(cut), y = n_recruits )) + geom_point()
# 
# 
# ms <- lm(n_stem ~ as.Date(cut) + median_dbh*n_recruits, data = dailyRate)
# summary(ms)


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


quadrats_with_error <- unique(allErrors[errorType %in% "error", quadrat])
quadrats_with_warnings <- unique(allErrors[errorType %in% "warning", quadrat])


quadrats <- quadrats %>%
  mutate(completion_status = case_when(PLOT %in%  intersect(quadrats_with_warnings, quadrats_with_error) ~ "warning & error pending",
                                       PLOT %in%  quadrats_with_error ~ "error pending",
                                       PLOT %in%  quadrats_with_warnings ~ "warning pending",
                                       
                                       PLOT %in%  stem$quadrat ~ "done"))

filename <- file.path(here("QAQC_reports"), "map_of_error_and_warnings.png")

png(filename, width = 9, height = 8, units = "in", res = 300)
par(mar = c(0,3,0,0))

ggplot() + 
  geom_sf(data = quadrats, aes(fill = completion_status)) + 
  scale_fill_manual(values = c("done" = "grey", "warning pending"= "yellow", "error pending" = "orange", "warning & error pending" = "red")) + theme_void()

dev.off()



# save quadrats that don't have any error ####

stemToSave <- stem[!quadrat %in% allErrors$quadrat, ]

write.csv(stemToSave, "processed_data/scbi.stem4.csv", row.names = F)

