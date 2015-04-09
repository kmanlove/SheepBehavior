DirectContactsByGrpsz <- function(data.in) {
  # plots and models relationship between direct contacts and group size
  #
  # Args
  # 
  # data.in = session-level focal follow data
  #
  # Returns
  #
  # Plot of relationship between contact rate and groupsize
  #

  
  par(mfrow = c(1, 3))
  plot(log(data.in$TotContactDur + 1) ~ data.in$sessionGrpSz, ylab = "Log(Total contact duration + 1)", 
       xlab = "Group size")
#  lines(lowess(log(TotContactDur + 1) ~ sessionGrpSz))
  plot(log(data.in$ELContactDur + 1) ~ data.in$sessionGrpSz, ylab = "Log(EL contact duration + 1)", 
       xlab = "Group size")
#  lines(lowess(log(ELContactDur + 1) ~ sessionGrpSz))
  plot(log(data.in$LLContactDur + 1) ~ data.in$sessionGrpSz, ylab = "Log(LL contact duration + 1)", 
       xlab = "Group size")
#  lines(lowess(log(LLContactDur + 1) ~ sessionGrpSz))
  test.fit.linear <- glm(data.in$TotContactDur ~ data.in$sessionGrpSz, family = "poisson")
  test.fit.quad <- glm(data.in$TotContactDur ~ data.in$sessionGrpSz + I(data.in$sessionGrpSz ^ 2), 
                       family = "poisson")
  anova.out <- anova(test.fit.linear, test.fit.quad, test = "F")
  
  return(list(test.fit.linear = test.fit.linear, test.fit.quad = test.fit.quad, anova.out = anova.out))
}