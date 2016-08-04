Optimize <- function (obj, startpar = obj$par, lower = rep(-Inf, length(startpar)), 
    upper = rep(Inf, length(startpar)), getsd = TRUE, control = list(eval.max = 10000, 
        iter.max = 10000, trace = TRUE), savedir = NULL, loopnum = 3, ...)  
{
    start_time = Sys.time()

    opt = nlminb(start = startpar, objective = obj$fn, gradient = obj$gr, 
        control = control, lower = lower, upper = upper)

    for (i in seq(2, loopnum, length = max(0, loopnum - 1))) {
        opt = nlminb(start = opt$par, objective = obj$fn, gradient = obj$gr, 
            control = control, lower = lower, upper = upper)
    }

    opt[["run_time"]] = Sys.time() - start_time

    opt[["diagnostics"]] = data.frame(Param = names(obj$par), 
        starting_value = startpar, Lower = lower, MLE = opt$par, 
        Upper = upper, final_gradient = obj$gr(opt$par))

    if (getsd == TRUE) {
        rep <- sdreport(obj, ...)
        opt[["SD"]] = rep
        opt[["Random"]] <- summary(rep, 'random')
    }  
  
    if (!is.null(savedir)) {
        save(opt, file = file.path(savedir, "opt.RData"))
        capture.output(opt, file = file.path(savedir, "opt.txt"))
    }
    opt
}
