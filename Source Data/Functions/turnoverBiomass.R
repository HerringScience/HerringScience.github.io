right = function (string, char) {
  substr(string, nchar(string) - (char - 1), nchar(string))
}

left = function (string, char) {
  substr(string, 1, char)
}

datediff = function(date2, date1) {
  if(
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    )))) < 0
  ){
    0
  } else {
    as.numeric(gsub("S", "", gsub("d", "", left(
      as.character(days(date2) - days(date1)), 2
    ))))
  }
}

turnoverBio = function(y_intercept,
                       x_Var_1,
                       daysturnover,
                       Date,
                       Survey,
                       Biomass) {
  turnMat <-
    matrix(nrow = (length(Date) - 1), ncol = (length(Survey) -
                                                1))
  
  for (j in 1:(length(Survey) - 1)) {
    for (i in 1:(length(Date) - 1)) {
      turnMat[i, j] <-
        if (datediff(Date[i + 1], Date[j]) >= daysturnover) {
          turnMat[i, j] <- 0
        } else  if ((1 - (x_Var_1 * log10(
          datediff(Date[i + 1], Date[j])
        ) + y_intercept))*Biomass[Survey[j]] < 0) {
          turnMat[i, j] <- 0
        } else {
          turnMat[i, j] <- (1 - (x_Var_1 * log10(
            datediff(Date[i + 1], Date[j])
          ) + y_intercept)) * Biomass[Survey[j]]
        }
    }
  }
  turnMat[turnMat == Inf] = 0
  finalbiomass <-
    Biomass[2:length(Biomass)] - rowSums(turnMat)
  finalbiomass[finalbiomass < 0] = 0
  totalbiomass <-
    as.integer(sum(c(Biomass[1], finalbiomass)))
  return(totalbiomass)
}