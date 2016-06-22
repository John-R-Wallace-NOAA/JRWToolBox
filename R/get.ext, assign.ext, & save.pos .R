get.ext <- function(x, file) {

  attach(file, pos = 2, warn.conflicts = F)
  on.exit(detach(2))
  get(deparse(substitute(x)), pos = 2)
}


assign.ext <- function(x, file, ...) {

  if(!file.exists(file))
     save(file=file) 

  attach(file, pos = 2, warn.conflicts = F)
  on.exit(detach(2))
  assign(deparse(substitute(x)), x, pos=2)
  save(list=base::ls(pos=2, all.names=T), file=file, ...)
  
}


ls.ext <- function(file) {
   local({
       base::load(file)
       base::ls()
    })
 }


  con <- 
## print the value to see what objects were created.
print(load(url("http://some.where.net/R/data/example.rda")))
close(con) # url() always opens the connection



save.pos <- function(pos, path=NULL, ...) {

  if(is.null(path)) {
     path <- attr(as.environment(search()[pos]), 'path')
     if(is.null(path)) 
        return("A path is needed for saves that don't have a path attribute as library packages do.")
  }

  save(list=base::ls(pos=pos, all.names=T), file=paste(path, "/.RData", sep=""), ...)
}


if(F) {

get.ext(imap, "W:\\ALL_USR\\JRW\\BATHYMET.R\\.Data\\Imap\\.RData")



assign.ext(GXX, "W:\\ALL_USR\\JRW\\BATHYMET.R\\.Data\\Imap\\.RData")


get.ext(GXX, "W:\\ALL_USR\\JRW\\BATHYMET.R\\.Data\\Imap\\.RData")


"W:\\ALL_USR\\JRW\\BATHYMET.R\\.Data\\Imap\\.RData"



assign.ext(get.ext, "W:/ALL_USR/JRW/R/library/JRW Functions Source/get.ext & assign.ext.RData")
assign.ext(assign.ext, "W:/ALL_USR/JRW/R/library/JRW Functions Source/get.ext & assign.ext.RData")



get.ext(get.ext, "W:/ALL_USR/JRW/R/library/JRW Functions Source/get.ext & assign.ext.RData")


save.pos(3)

save.pos(4)


}
