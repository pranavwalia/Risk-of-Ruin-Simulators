library(ggplot2)
####Outputs a dataFrame logging n trials of a bet using the Kelly Criterion
#capital[double]: Starting value
#p[double]: probability of success (passed as a percent eg. 1,2,50)
#[trials]: sample size of trials
#[odds]: The odds of the bet. 1:1 => you make back exactly what you bet. 2:1 => twice what you bet
#kelly[boolean] value representing whether or not to use kelly criterion
runKellySim <- function(capital,p,trials,odds,kelly=TRUE,betPercent=.01) {
  out <- data.frame(matrix(ncol = 2, nrow = 0))
  cols <- c("Trials","Capital")
  
  for (i in 1:trials) {
    moneyToBet = 0
    #uses kelly formula to determine percentage of portfolio to bet
    if (kelly) {
      moneyToBet = (((odds * (p/100)) - (1 - (p/100)))/odds) * capital
    } else {
      moneyToBet = betPercent * capital
    }
    
    #Random number to simulate bet outcome
    t <- sample(1:100,1)
    if (t <= p) {
      capital <- capital + (odds * moneyToBet)
    } else {
      capital <- capital - moneyToBet
    }
    out <- rbind(out,c(i,capital))
    
    #If we run out of money, end the sim
    if (capital <= 0) {
      break
    }
  }
  colnames(out) <- cols
  out
}


###Returns the risk of ruin for a particular kelly simulation
#sampleSize[int] how many kelly simulations to run
#kelly: boolean value whether to use kelly method or not
measureRisk <- function(sampleSize,capital,p,trials,odds,kelly,betPercent=.01) {
  ruins <- 0
  for (i in 1:sampleSize) {
    
    s <- c()
    if (kelly) {
      s <- runKellySim(capital,p,trials,odds)
    } else {
      s <- runKellySim(capital,p,trials,odds,FALSE,betPercent)
    }
    if (nrow(s) < trials) {
      ruins <- ruins + 1
    }
    print(paste("Sim",toString(i),"Complete"))
  }
  ruins/sampleSize
}



simulation <- runKellySim(1000,51,10000,1)
risk <- measureRisk(100,1000,51,10000,1,TRUE)

ggplot() + geom_line(data=simulation,aes(x = simulation$Trials,y = simulation$Capital)) + xlab("Trials") + ylab("Capital")


