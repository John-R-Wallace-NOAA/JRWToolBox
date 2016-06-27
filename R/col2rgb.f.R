col2rgb.f <- function (color, alpha = 1) 
{
    COL <- col2rgb(color)/255
    rgb(red = COL[1], green = COL[2], blue = COL[3], alpha = alpha)
}
