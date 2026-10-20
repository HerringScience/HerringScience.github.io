

library(rerddap)

rerddap::servers()

## set url to use with other functions
url <- "https://cioosatlantic.ca/erddap/"
# get datasets on the server
datasets <- rerddap::ed_datasets(which = 'tabledap',
                                 url = url)

head(datasets)

datasetid <- "bio_maritimes_region_ecosystem_survey_ctd"
datasetid <- "bio_atlantic_zone_monitoring_program_ctd"
datasetid <- "bio_atlantic_zone_monitoring_program_moored_ctd"

#"bio_atlantic_zone_monitoring_program_ctd"

rerddap::browse(x = datasetid, url = url)

info <- rerddap::info(datasetid = datasetid, url = url)

info

info$alldata$TEMPP901

# Quering the data:
alldata <- tabledap(x = info,
                    url = url)
info$alldata$time

time1 <- as.numeric(as.POSIXct('2024-01-01'), origin = '1970-01-01', tz = 'UTC')
time2 <- as.numeric(as.POSIXct('2024-12-31'), origin = '1970-01-01', tz = 'UTC')
query1 <- paste0('time>=', time1)
query2 <- paste0('time<=', time2)

vars <- info[['variables']][['variable_name']]

okmetavars <- !grepl(pattern = '.*\\d$', 
                     x = vars)
vars[okmetavars]
# we get station here...unique

## primary variables end with a '1'
okprimary <- grepl(pattern='.*1$',
                   x = vars)
vars[okprimary]

## now combine them together
fields <- c(vars[okmetavars],
            vars[okprimary])

# query
d <- tabledap(x = info,
              fields = fields,
              query1,
              query2,
              url = url)

# for the 'primary' variables found above, they contain the quality control
#   variables as well, let's get only the data variables
allPrimaryVars <- vars[okprimary]
allPrimaryVars

primaryDataVars <- allPrimaryVars[!grepl(pattern = '^Q.*', x = allPrimaryVars)]
primaryDataVars

dfqcvars <- NULL

for(ip in 1:length(primaryDataVars)){
  var <- primaryDataVars[ip]
  ok <- which(names(info$alldata) == var)
  if(length(ok) != 0){
    dvar <- info[['alldata']][[ok]]
    if('ancillary_variables' %in% dvar[['attribute_name']]){
      okattribute <- which(dvar[['attribute_name']] == 'ancillary_variables')
      dfadd <- data.frame(variable = var,
                          qcVariable = dvar[['value']][okattribute])
      dfqcvars <- rbind(dfqcvars, dfadd)
    }
  }
}  


# apply qc flags to data
## look at one of the qc variables, this gives us information about the flags
info$alldata$QTEMP_01

# the flag_meanings and flag_values help us understand what they mean
qcvareg <- info$alldata$QTEMP_01
okmeaning <- which(qcvareg[['attribute_name']] == 'flag_meanings')
okvalue <- which(qcvareg[['attribute_name']] == 'flag_values')
flagMeaning <- data.frame(value = trimws(strsplit(qcvareg[['value']][okvalue], ',')[[1]]),
                          meaning = strsplit(qcvareg[['value']][okmeaning], split = ' ')[[1]])

#value	meaning
#0	not_evaluated
#1	correct
#2	inconsistent_with_other_values
#3	doubtful
#4	erroneous
#5	modified
#9	missing

# we'll keep any flags = 0 and 1
## re-name queried data so the original query is retained
dd <- d
for(i in 1:dim(dfqcvars)[1]){
  datavar <- dfqcvars[['variable']][i]
  qcvar <- dfqcvars[['qcVariable']][i]
  # get variable data
  data <- d[[datavar]]
  # get qc data
  qcdata <- d[[qcvar]]
  # apply qc
  data[!qcdata %in% c(0,1)] <- NA
  # add qc'd data to queried data
  dd[[datavar]] <- data
}

#d      # original data
#↓
#apply QC using dfqcvars
#↓
#dd     # cleaned/QC'd data

head(dd)
colnames(dd)
unique(dd$station)

prince5 <- dd[dd$station == "Prince5", ]
unique(prince5$time)


getwd()
write.csv(prince5, "Prince5_AZMP.csv", row.names = FALSE)


### Look at 2024 casts:

library(dplyr)
library(tidyr)
library(ggplot2)


# plot to show temp and salinity for each cast
# each cast has a downwards and upwards recording. I have to determine how these are typically combined by DFO.
profiles <- prince5 %>%
  select(event_number, depth, TEMPS901, PSALST01) %>%
  mutate(
    depth = as.numeric(depth),
    TEMPS901 = as.numeric(TEMPS901),
    PSALST01 = as.numeric(PSALST01)
  ) %>%
  pivot_longer(
    cols = c(TEMPS901, PSALST01),
    names_to = "variable",
    values_to = "value"
  )

ggplot(profiles,
       aes(x = value, y = depth, colour = factor(event_number))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    x = NULL,
    y = "Depth (m)",
    colour = "Cast"
  ) +
  theme_bw()
