files <- list.files("./data/", full.names = T)[1:2]

lapply(files, file.remove)
