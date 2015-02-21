save2file <- function(
    str = NULL,
    root = NULL,
    dir.name = NULL,
    file.name = NULL
){
    oldwd <- getwd()

    tmp.dir <- sprintf("%s/%s",root,dir.name)
    dir.create(tmp.dir)
    setwd(tmp.dir)
    
    sink(file.name)
    cat(str)
    sink()

    setwd(oldwd)
    return(str)
}
