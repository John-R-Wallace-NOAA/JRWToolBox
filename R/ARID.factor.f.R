ARID.factor.f <- function(x, labels = c("1A", "1B", "1C", "2A", "2B", "2C", "3A", "3S_&_3B"))
  {
     # Note: Going all the way to the U.S./Mexican EEZ not the official Southern Border of the 
     #       1A at 32.5 degrees latitude.

     # If a category has no latitudes within it, then that cateogory label neeeds to be removed, e.g.:
     #         table(ARID.factor.f(Lat.DD, labels = c("1A", "1B", "1C", "2A", "2B", "2C", "3A", "3S_&_3B")[-(1:3)])

      factor.f(x, breaks = c(32.0, 36, 40.5, 42.0, 42.8333333, 44.30, 45.7666667, 47.3333333, 48.43333), labels = labels)
       
   }


