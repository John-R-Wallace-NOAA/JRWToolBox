

fractal_zoom <- function(center = c(-0.5, 0), width = 3, resolution = 500, max_iter = 100, escape_radius = 4) {

 '   # Created by DeepSeek which incorrectly used locator() and not ggmap::gglocator()   '
 '   # Click two points to zoom in, right-click to exit   '

  library(ggplot2)
  library(ggmap)

  
  # Initialize plot parameters
  xlim <- c(center[1] - width/2, center[1] + width/2)
  ylim <- c(center[2] - width/2, center[2] + width/2)
  
  while(TRUE) {
    # Create coordinate grid
    x <- seq(xlim[1], xlim[2], length.out = resolution)
    y <- seq(ylim[1], ylim[2], length.out = resolution)
    
    # Generate Mandelbrot set
    mandelbrot <- function(x, y, max_iter, escape_radius) {
      c <- complex(real = x, imaginary = y)
      z <- 0
      for(i in 1:max_iter) {
        z <- z^2 + c
        if(Mod(z) > escape_radius) return(i)
      }
      return(max_iter)
    }
    
    # Calculate iterations matrix
    iterations <- matrix(0, nrow = resolution, ncol = resolution)
    for(i in 1:resolution) {
      for(j in 1:resolution) {
        iterations[i,j] <- mandelbrot(x[i], y[j], max_iter, escape_radius)
      }
    }
    
    # Create data frame for plotting
    df <- expand.grid(x = x, y = y)
    df$value <- as.vector(iterations)
    
    # Plot using ggplot2
    p <- ggplot(df, aes(x, y, fill = log(value + 1))) +
      geom_raster() +
      scale_fill_viridis_c(option = "plasma") +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none")
    
    print(p)
    
    # Get user input for zoom
    cat("Click two points to define zoom region (right-click to exit)\n")
    zoom_coords <- gglocator(2)
    
    if(is.null(zoom_coords)) break  # Exit if right-clicked
    
    # Update plot boundaries
    xlim <- sort(c(zoom_coords$x[1], zoom_coords$x[2]))
    ylim <- sort(c(zoom_coords$y[1], zoom_coords$y[2]))
    width <- diff(xlim)
  }
}

# Usage example:
# fractal_zoom()  # Initial view
# Click two points to zoom in, right-click to exit
