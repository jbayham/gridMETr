source("download_data.R")

gridmetr_download(variables = c("pdsi","erc"),
                  years = seq.int(2000,2002),
                  parallel.workers = 6)
