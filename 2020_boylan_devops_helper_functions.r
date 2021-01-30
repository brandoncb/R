# library(tidyverse)
# library(lubridate)

# get most recent csv file
getNewestCSV <- function(root, dir, ext){
  mypath <- paste(root, dir, ext, sep = "") # "____/*.csv"
  data_files <- file.info(Sys.glob(mypath))
  newest_csv <- row.names(data_files)[which.max(data_files[["ctime"]])]
  return(newest_csv)
}

root <- "/gne/data/obdroot/_bodi_/"
ext = "/*.csv"


# icon <- getNewestCSV(root, "GO39775_ICON_RESULTS_Prelim", ext) # need to do a static file pull due to issues between OBDFS and BODI Valicert ICON Results files, and the 20201203 OBDFS file is what Rin used in development
icon <- "/gne/data/obdroot/fcrh5_cd3_tdb/go39775/bodi/GO39775_Analyte Trend Report_20201203.csv"

irr <- getNewestCSV(root, "____", ext)

lym <- getNewestCSV(root, "____", ext)

sls <- getNewestCSV(root, "____", ext)



######################
######################
######################


# create a unique row identifier
df$TableauID <- cumsum(!duplicated(df[1:dim(df)[2]]))

# split columns in half; subtract two to make file sizes more equal
first_half <- round(dim(df)[2]/2) - 2

# split datasets in half by columns and keep the unique row identifier in both so they can be re-joined in Tableau
df_first_half <- df[, c(1:first_half,dim(df)[2])]
df_second_half <- df[, (first_half + 1):dim(df)[2]]


write.csv(df_first_half, "/gstore/home/dsisys44/bodi/GO39775/GO39775_Rin_Nakamura_flow_first_half.csv", row.names=FALSE)
write.csv(df_second_half, "/gstore/home/dsisys44/bodi/GO39775/GO39775_Rin_Nakamura_flow_second_half.csv", row.names=FALSE)
write.csv(df, "/gne/data/obdroot/fcrh5_cd3_tdb/go39775/bodi/GO39775_Rin_Nakamura_flow.csv", row.names=FALSE)
