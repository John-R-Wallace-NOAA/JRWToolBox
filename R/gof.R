gof <- 
function () 
{
    while ((which <- dev.cur()) != 1) dev.off(which)
    if (exists('.SavedPlots')) rm(.SavedPlots, pos = 1)
}
