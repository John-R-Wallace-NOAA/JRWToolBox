vessel.f <- function (x) 
{
    round(1000 * dec(trunc(x/1000)/1000))
}
