files <- list.files("./data/", full.names = T)

lapply(files, file.remove)
