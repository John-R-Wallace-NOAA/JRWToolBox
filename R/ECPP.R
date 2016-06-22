ECPP <- function(N=657, COL.FUNC = Colors, pick= !tones, tones = F, Repeat = 1, delay = 1) {

#     ///////////////////////////////////////////////////////// 
#    //                                                      //
#    //    Eliza's Fantastical World of Color and Sound      //
#    //                                                      //   
#     ////////////////////////////////////////////////////////  

   # Example of tones- the DOS executable 'xecho' is needed for 'tones' to work.
   # The color picking works independently of the tones working.
      
   # ECPP(tones="A4G4J5N5R6S5X5Y4Z4Z5d6g6h5h5h4i3f3d2Z1U0K0K2M7P7J5L5M3U")
   # ECPP(tones="A5Q5T4O4Q4T4W4Y4b4Y4W4T4Q4N7L")

        sort.f <- function(x, col = 1, reverse = F, renumber = T)
        {
            sort.col <- x[, col, drop=F]

            if(is.factor(sort.col))
                sort.col <- as.character(sort.col)

            if(!(is.list(sort.col)))
                sort.col <- list(sort.col)

            if(reverse)
                out <- x[rev(do.call(order, sort.col)),  ]
            else 
                out <- x[do.call(order, sort.col),  ]

            if(renumber) {
                if(is.data.frame(x))
                        dimnames(out)[[1]] <- 1:nrow(out)
            }
            out
        }

     timer <- function(secs, silent=F) 
     {
       T0 <- Sys.time()

      if(!silent)
       print(T0); flush.console()    

      while(secs > Sys.time() - T0)   "Wait"
    
      T1 <- Sys.time()

      if(!silent) {
       print(T1)
       cat("\n", T1 - T0, "seconds have passed\n\n")
     }

     invisible(T1 - T0)
    }


# ---------------------------------------------------

        Colors <- function (N) 
        {
            Nvec <- (1:N) %% 657
            vec<- ifelse(Nvec == 0, 657, N) 
            colors()[Nvec]
        }

        if(!is.logical(tones)) {
           for (i in 1:Repeat) {
             
             system(paste("xecho #[", tones, "]", collapse = ""), wait = F)
             timer(delay, silent=T)
           }
            return(invisible())
        }

        n <- ceiling(sqrt(N))
        index <- expand.grid(1:n, 1:n)

        plot(index, xlab = "", ylab = "", col=COL.FUNC(N)[(index[,2] - 1) * n + index[,1]], pch=19, cex=3)

        if(pick) {      

           return(COL.FUNC(N)[ID <- identify(index, label=COL.FUNC(N)[(index[,2] - 1) * n + index[,1]])])

        }
           
        if(tones) {
            
           Notes <- "!"    # First note not played by xecho.com when called from R
           while(T) {

              XY <- locator(1)
              if(is.null(XY)) break

              X <- ( c("_", "!", "\"", "$", "&", "'", "(", ")", "*", "+", "-", ".", "/", ":", ";", "=", "?", "@",
                     LETTERS, letters))[round(70*XY$x/n)]
              Y <- c(0:9, "{", "}", "~", "\\", "^", "`")[round(16 * XY$y/n)]
            
              system(paste("xecho #[", "A", Y, X, "]", sep = ""))              

              Notes <- paste(Notes, Y, X, sep = "")
              
          }
              print(Notes); flush.console()
              system(paste("xecho #[", Notes, "]", sep = ""))
              invisible(Notes)
          }
}





