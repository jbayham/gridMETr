source("functions/download_data.R")

gridmetr_download(variables = c("pdsi","erc"),
                  years = seq.int(2000,2001))
