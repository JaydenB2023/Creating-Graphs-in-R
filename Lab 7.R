##Lab 7: Advanced Plotting
#Jayden Brown 2/23/22

library(readr)
Child <- read_csv("InfantMortality2017.CSV")

# assign independent and dependent variables
x <- (na.omit(Child$welfare)/1000) #ind var
y <- (na.omit(Child$inf.mort)) #dep var

# Start with blank plot
plot(x, y,
     type ="n", #no points
     axes = FALSE,
     xlab = "",
     ylab = "",
     xlim = c(0,40),
     ylim = c(0,10))
# Add axes
axis(1)
axis(2, las = 1)
# Add a title and labels
title(xlab = "Welfare Spending per Person in Poverty (in thousands)",
      ylab = "Infant Mortality Rate per 1000 Live Births",
      main = "Impact of Welfare Spending on Infant Mortality
     Rates in the U.S. 2017")
# Add box around plot
box()
text(x, y, labels = Child$postal.code, cex =.7)

#regression line
reg <- lm(y~x)
abline(reg, col = "red")

#Plot R
r <- cor(x,y)
newr <- round(r, digits = 2)
text(5,2, paste("r = ", newr))

## Time-series plot
turnout <- read_csv("RealInc.csv")
y <- turnout$real.disp.pers.inc
x <- turnout$year
plot(x, y,
     type ="l", #no points
     axes = FALSE,
     xlab = "",
     ylab = "",
     xlim = c(1980, 2020),
     ylim = c(-2, 8))
# Add axes
axis(1)
axis(2, las = 1)
# Add a title and labels
title(ylab = "Percent Change",
      main = "Percent Change in Real Disposable Personal Income, 1982-2017")

#add rectangle
rect(1982, -2, 1990, 8, col="grey", border = NA)
rect(1994, -2, 2002, 8, col="grey", border = NA)
rect(2010, -2, 2017, 8, col="grey", border = NA)

#add lines
lines(x, y, col = "red", lwd = 2)

# Add text
text(1986, 7.5, paste("Reagan"), cex = 0.6)
text(1992, 7.5, paste("Bush (41)"), cex = 0.6)
text(1998, 7.5, paste("Clinton"), cex = 0.6)
text(2006, 7.5, paste("Bush (43)"), cex = 0.6)
text(2013, 7.5, paste("Obama"), cex = 0.6)
