
resultTableC = function(x) {

C = data.frame(x$TS, x$areaKm, x$meanSa, x$mean_biomass_density, x$total_biomass, x$standard_error_tonnes, x$standard_error_perc) 

C = C[1,]

colnames(C) = c("Target Strength", "Area (km2)", "Mean Sa", "Density (kg/m2)", "Biomass (tons)","Standard Error (tons)", "Standard Error (%)")

return(C)
}