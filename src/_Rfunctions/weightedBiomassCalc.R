
weightedBiomassCalc = function(x, areaKm) {
  
  x$areaKm = areaKm
  
  # Analysis
  x$sum_trans =sum(x$Dist_T)
  x$Actual_Weighting = (x$Dist_T)/(x$sum_trans)
  x$calc_actual_mean_sa = 10^(x$Area_Backscatter_Strength/10)*x$Actual_Weighting
  x$density = (x$biomass_density)*(x$Dist_T)
  x$weighted_mean_biomass_calc = (x$biomass_density) * (x$Actual_Weighting)
  x$trans_biomass = (x$weighted_mean_biomass_calc) * x$areaKm * 1000
  x$total_biomass = sum(x$trans_biomass)
  
  se <- function(x) sqrt(var(x)/length(x))
  x$se = se(x$biomass_density)
  x$standard_error_tonnes = x$se * x$areaKm * 1000
  x$standard_error_perc = x$standard_error_tonnes/x$total_biomass*100
  x$mean_biomass_density = mean(x$biomass_density)
  x$meanSa = 10*log10(sum(x$calc_actual_mean_sa))
  
    return(x)
}