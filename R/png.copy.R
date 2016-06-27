function (file, factor.exp = 1, ...) 
{
    dev.copy(png, file = file, width = par()$fin[1] * factor.exp, 
        height = par()$fin[2] * factor.exp, units = "in", res = 72, 
        ...)
    dev.off()
}
