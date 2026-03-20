
#code to answer the question:
#within a bin and ground, does unequal Julian day coverage bias the temperature comparison between HSC and DFO?
# Can change the data set, temp_var. avgTemp, avgDensity, etc.

#function: within bin julian day bias test 

source("test_within_bin_julian_bias.R")

colnames(surface)


# run it
# change data type or variable
within_bin_bias <- test_within_bin_julian_bias(
  data   = deep,
  temp_var = "avgSoundSpeed",
  min_n = 10,
  use_gam = TRUE
)

within_bin_bias


write.table(within_bin_bias, file = "within_bin_bias_deep_avgSoundSpeed.csv",sep = ",", quote=FALSE, row.names=FALSE, col.names=TRUE)
  

