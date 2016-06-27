Plot.SSB.3.Models <- function (M1, M2, M3, Years = All.Years, Areas = NULL, Depl = F, 
    ylim = NULL, ylim.depl = c(0, 100), gp = 0) 
{
    if (is.null(M1$timeseries$Area)) 
        Num.Area <- 1
    else Num.Area <- length(unique(M1$timeseries$Area))
    All.Years <- unique(M1$timeseries$Yr)
    NY <- length(All.Years)
    N <- nrow(M1$timeseries)
    if (is.null(Areas)) 
        Areas <- 1:Num.Area
    for (i in 1:length(Areas)) {
        I <- M1$timeseries$Area %in% Areas[i] & M1$timeseries$Yr %in% 
            Years
        YI <- NY * (Areas[i] - 1) + 1
        if (i == 1) {
            if (Depl) 
                plot(M1$timeseries$Yr[I], 100 * M1$timeseries$SpawnBio[I]/M1$timeseries$SpawnBio[YI], 
                  type = "o", col = "blue", ylim = ylim.depl, 
                  xlab = "Year", ylab = "Spawning depletion")
            else {
                if (is.null(ylim)) 
                  ylim <- c(0, max(100 * c(M1$timeseries$SpawnBio[I], 
                    M2$timeseries$SpawnBio[I], M3$timeseries$SpawnBio[I])))
                plot(M1$timeseries$Yr[I], 100 * M1$timeseries$SpawnBio[I], 
                  type = "o", col = "blue", ylim = ylim, xlab = "Year", 
                  ylab = "Spawning biomass")
            }
        }
        else points(M1$timeseries$Yr[I], 100 * M1$timeseries$SpawnBio[I]/ifelse(Depl, 
            M1$timeseries$SpawnBio[YI], 1), type = "o", col = col2rgb.f("blue", 
            1/i^gp))
        points(M2$timeseries$Yr[I], 100 * M2$timeseries$SpawnBio[I]/ifelse(Depl, 
            M2$timeseries$SpawnBio[YI], 1), type = "o", col = col2rgb.f("red", 
            1/i^gp), pch = 4)
        points(M3$timeseries$Yr[I], 100 * M3$timeseries$SpawnBio[I]/ifelse(Depl, 
            M3$timeseries$SpawnBio[YI], 1), type = "o", col = col2rgb.f("green", 
            1/i^gp), pch = 23)
    }
    abline(h = 0, col = "grey")
    abline(h = c(25, 40))
}
