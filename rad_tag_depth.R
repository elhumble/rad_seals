# read process_radtag log files and determine depth of coverage per individual

require(dplyr)

temp_logs <-list.files(pattern="*.log")

log2csv <- function(file){
  file <- readLines(file, n =29)
  file <- file[c(13:29)]
}

logs <- lapply(temp_logs, log2csv)

dir.create("temp/")

lapply(1:length(logs), function(i) write.table(logs[[i]], 
                                                file = paste("temp/", temp_logs[i],".csv", sep = ""),
                                                sep = "\t", row.names = F, col.names = F, quote = F))

temp_csvs <- list.files(path = "temp", pattern="*.log.csv")
temp_csvs <- lapply(temp_csvs, function(x) paste("temp/", x, sep=""))

read.log <- function(file){
  read.csv(file, sep = "\t", header = T) %>%
    select(Barcode:Retained) %>%
    mutate(pool = paste(substitute(file)))
}

log <- do.call(rbind, lapply(temp_csvs, read.log)) %>%
  mutate(percent.retained = Retained/Total) %>%
  group_by(pool) %>%
  mutate(pool.fraction = Total / sum(Total), cov.per.tag = Retained/80000)

hist(log$cov.per.tag, breaks = 40)
