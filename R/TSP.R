

TSP <- function(funds = Allfunds, plot = TRUE, grid = TRUE) {

   " #  Subset of funds: TSP(c('L 2020', 'L 2030', 'L 2040')) "

   Allfunds <- c("L Income", "L 2020", "L 2030", "L 2040", "L 2050", "G Fund", "F Fund", "C Fund", "S Fund", "I Fund")
 
   tempFile <- tempfile()
   curl_download("https://www.tsp.gov/InvestmentFunds/FundPerformance/index.html", tempFile)
   TSPData <- scan(tempFile, "")

   Prices <- matrix(as.numeric(TSPData[grep("class='packed'",  TSPData) + 1]), ncol = 10, byrow=T)
   Months <- substring(TSPData[grep("class='leadingCell'",  TSPData)][-1], 21)
   Days <- substring(TSPData[grep("class='leadingCell'",  TSPData) + 1][-1],1,2)
   Years <- substring(TSPData[grep("class='leadingCell'",  TSPData) + 2][-1],1,4)
   Dates <- as.Date(paste(Years, Months, Days, sep="-"), "%Y-%b-%d")

   TSPShares <- data.frame(Date = Dates, Prices)
   
   names(TSPShares)[2:11] <- Allfunds

   if(plot) {

      TSPPlot <- data.frame(Date = rep(Dates, 10), Price = c(Prices), Funds = rep(Allfunds, each = 25))
      DATA <- TSPPlot[TSPPlot$Funds %in% funds,]
      DATA$Funds <- factor(DATA$Funds, Allfunds, ordered = TRUE)
     
      Col <- c("red", "green", "blue", "cyan", "purple" ,"grey", "orange", "hotpink", "brown", "darkolivegreen2")
      windows(15.5, 8)

      print(xyplot(Price~Date, groups = Funds, data = DATA, type= 'o', ylab = "Share Price ($)", grid = grid, col = Col,
                       key=list(text=list(levels(DATA$Funds)), space='top', points=list(pch=rep(1, nlevels(DATA$Funds)), col=Col[1:nlevels(DATA$Funds)]), 
                                columns=nlevels(DATA$Funds))))
   }

   TSPShares
}  

