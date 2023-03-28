# Generate warnings looking at the reports that are not looked at with testhat ####

## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)

# load files ####

will_auto_fix_error_file_path <- file.path(here("testthat"), "reports/will_auto_fix/will_auto_fix_error_file.csv")
if(file.exists(will_auto_fix_error_file_path)) will_auto_fix_error_file <- read.csv(will_auto_fix_error_file_path)



warning_file_path <- file.path(here("testthat"), "reports/warnings/warnings_file.csv")
if(file.exists(warning_file_path)) warning_file <- read.csv(warning_file_path)


# write warning messages ####

warning_messages <- c("unhealthy_but_wrong_status" = "There are living trees that are unhealthy but status is not AU.",
                      "wounded_level_but_wrong_status_or_FAD" = "There are trees with wounded level but no W in FAD.", 
                      "canker_level_but_wrong_status_or_FAD" = "There are trees with canker level but no K in FAD.",
                      "rot_level_but_wrong_status_or_FAD" = "There are trees with rot level but no R in FAD",
                      "epicormic_growth_but_not_AU" = "There are trees with epicormic growth but status is not AU.",
                      
                      "DBH_dead_suspicious" = "There are DBH measurements of dead trees that are not withing 2cm of previous census.", 
                      "Dead_but_now_alive" = "There are trees that are alive but were previously dead.",
                      "DC_but_now_A_AU_or_DS" = "There are trees that were DC but now A, Au or Ds.")


# check if files exist and generate a plot with the warning ####

if(exists("will_auto_fix_error_file")) all_will_be_fixed <- paste(c("ERRORS THAT WILL AUTO FIX:\n", warning_messages[unique(will_auto_fix_error_file$error_name)]), collapse = "\n") else  all_will_be_fixed <- ""


if(exists("warning_file")) all_warns <- paste(c("WARNINGS!!!\n", warning_messages[unique(warning_file$warning_name)], "CLICK HERE TO GO TO FOLDER"), collapse = "\n") else all_warns = "No WARNINGS"


filename <- file.path(here("testthat"), "reports/warnings.png")


if(length(all_warns) == 0 & length(all_will_be_fixed) == 0)  file.remove(filename)

png(filename, width = 6, height = 0.7 + (0.2*length(c(gregexpr("\\n", all_warns)[[1]], gregexpr("\\n", all_will_be_fixed)[[1]]))), units = "in", res = 300)
par(mar = c(0,0,0,0))
plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
text(0,0.7, all_warns, col = "red", cex = 0.6)
text(0,0, all_will_be_fixed, col = "red", cex = 0.6, pos = 1)
# title("warnings!!!", col.main= "red", xpd = NULL, line = -1)
dev.off()

