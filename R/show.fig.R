function (width = 6.5, height = 5, units = "in", res = 150, ...) 
{
    FILE <- paste(".\\Figs\\Fig.", FIG.NUM, ".jpg", sep = "")
    printf(FILE)
    FIG.NUM <<- FIG.NUM + 1
    dev.copy(jpeg, file = FILE, width = width, height = height, 
        units = units, res = res, quality = 100, ...)
    dev.off()
    shell(paste("start", FILE))
}
