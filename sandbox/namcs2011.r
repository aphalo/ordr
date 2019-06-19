
# exogenous variables
exo_vars <- c(
  "year", "patwt",
  "age", "sex", "ethun", "raceun", "ager",
  "usetobac", "paytyper", "msa",
  "injdet", "senbefor",
  "phycode", "retypoff", "specr", "region"
)
# endogenous variables
endo_vars <- c(
  "arthrtis", "asthma", "cancer", "cebvd", "chf", "copd", "deprn",
  "diabetes", "hyplipid", "htn", "ihd", "obesity", "ostprsis"
)

# 2015 NAMCS
url <- paste0(
  "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
  "dataset_documentation/namcs/stata/namcs2011-stata.zip"
)
# temporary directory
temp_dir <- here::here("data-raw/temp")
if (! dir.exists(temp_dir)) dir.create(temp_dir)
# temporary file
temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")
# download
download.file(url, temp_file)
# unzip
unzip(temp_file, exdir = temp_dir)
temp_data <- file.path(temp_dir, "namcs2011.dta")
# read into data frame
namcs2011 <- foreign::read.dta(temp_data)
# remove temp file and directory
file.remove(temp_file)
file.remove(temp_data)
file.remove(temp_dir)

# lowercase variable names
#names(namcs2011) <- tolower(names(namcs2011))
# subset variables
#namcs2011 <- namcs2011[, c(exo_vars, endo_vars)]
# weighted sample of 12 physician practices
#phycode_table <- table(namcs2011$phycode)
#phycode_sample <- sample(names(phycode_table), 12, prob = phycode_table)
#namcs2011 <- subset(namcs2011, as.character(phycode) %in% phycode_sample)

library(dplyr)
# lowercase variable names
namcs2011 <- rename_all(namcs2011, tolower)
# subset variables
namcs2011 <- select(namcs2011, exo_vars, endo_vars)
# restrict to physician practices with at least 24 patients
namcs2011 %>%
  group_by(phycode) %>%
  add_count() %>%
  filter(n >= 24) ->
  namcs2011
# binarize chronic disorder indicators
namcs2011 <- mutate_at(
  namcs2011, vars(endo_vars),
  ~ ifelse(tolower(.) == "yes", 1L, 0L)
)
# summarize by practice
phy_vars <- c("phycode", "retypoff", "specr", "region")
namcs2011 %>%
  group_by_at(vars(phy_vars)) %>%
  summarize_at(vars(endo_vars), mean) ->
  namcs2011

# save data set
namcs2011 <- as.data.frame(namcs2011)
usethis::use_data(namcs2011)
