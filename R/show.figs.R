function () 
{
    dev.copy(jpeg, paste("W:\\Pics\\Pic.", PIC.NUM, ".jpg", sep = ""), 
        1680, 1050, quality = 100)
    dev.off()
    PIC.NUM <<- PIC.NUM + 1
    shell("start w:\\pics")
    invisible()
}
