
resultTableB = function(x) {


B = data.frame(x$Vessel, x$Transect_No, x$TS, x$Area_Backscatter_Strength, x$biomass_density,x$Dist_T) 

colnames(B) = c("Vessel", "Transect No.", "Target Strength (db/kg)", "Mean Sa (/m2)", "Biomass Density (kg/m2)", "Distance")

return (B)
}