

library(rerddap)

rerddap::servers()

## set url to use with other functions
url <- "https://cioosatlantic.ca/erddap/"
# get datasets on the server
datasets <- rerddap::ed_datasets(which = 'tabledap',
                                 url = url)

head(datasets)

datasetid <- "bio_maritimes_region_ecosystem_survey_ctd"
#"bio_atlantic_zone_monitoring_program_ctd"

rerddap::browse(x = datasetid, url = url)

info <- rerddap::info(datasetid = datasetid, url = url)

info

info$alldata$TEMPP901
