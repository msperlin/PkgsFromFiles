library(readr)
library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

folder.with.files <- '~/Dropbox/ODAFIN/YieldCurve/'

my.f <- list.files(folder.with.files, full.names = TRUE)

df.yield <- do.call(what = rbind, args = lapply(X = my.f, FUN = read_csv))


p <- ggplot(filter(df.yield, n.biz.days < 2500), aes(x = n.biz.days, y=value, frame = current.date, color = type)) + 
  geom_line(size=2) +  theme_bw() # +facet_wrap(~type, scales = 'free') 

library(gganimate)

gganimate(p, filename = 'YieldCurve.mp4' )
print(p)
