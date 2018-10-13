rm(list = ls())

library(XML)
library(ggplot2)
library(xlsx)
library(reshape2)
library(dplyr)
library(BatchGetSymbols)
library(readr)

save.df <- TRUE
format.out <- 'wide' # long or wide
format.out <- 'long' # long or wide
threshBadData <- 0.5
fix.na <- TRUE
bench.ticker <- '^BVSP'

myWd <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(myWd)

my.name <-'IbovComp'
last.date <- Sys.Date()
first.date <- last.date - 5*365
first.date <- as.Date('2015-01-01')

# get list of ibovespa's tickers from wbsite (does NOT work)

#myUrl <- 'http://www.infomoney.com.br/ibovespa/composicao'

myUrl <- 'http://bvmf.bmfbovespa.com.br/indices/ResumoCarteiraTeorica.aspx?Indice=IBOV&idioma=pt-br'
tables <- readHTMLTable(myUrl)[[1]]

tickers <- tables$`Código `
tickers <- c(paste0(tickers , '.SA'), '^BVSP')

# get list from xls

# my.f <- '2016.11.11 Ibovespa.xls'
# df <- XML::readHTMLTable(my.f)[[1]]
# colnames(df) <- c('codigo','acao','tipo','qtd','part')
# tickers <-as.character(df$codigo)
# tickers <- paste(tickers, '.SA', sep = '')
# tickers <- tickers [1:(length(tickers )-2)]
# tickers <- c(tickers, bench.ticker)

# download each data

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = first.date,
                         last.date = last.date,
                         bench.ticker = bench.ticker, thresh.bad.data = threshBadData )

l.out$df.control

df.tickers <- l.out$df.tickers

# fix tickers
df.tickers$ticker <- stringr::str_replace(df.tickers$ticker, '.SA', '')
df.tickers$ticker[df.tickers$ticker == '^BVSP'] <- 'BVSP'

# fix NA
df.tickers.wide <- dcast(data = df.tickers, formula = ref.date ~ ticker,value.var = 'price.adjusted')

if (fix.na){

  fix.na.fct <- function(x){

    if (!is.numeric(x)) return(x)

    x2 <- x[!is.na(x)]
    x[length(x)] <- x2[length(x2)]

    for (i in seq(length(x)-1,1)){
      if (is.na(x[i])) x[i] <- x[i+1]
    }

    return(x)
  }

  df.tickers.wide <- as.data.frame(lapply(df.tickers.wide, fix.na.fct))

  # fix NA Values
  df.tickers  <- tidyr::gather(data = df.tickers.wide ,
                               key = 'ticker',
                               value = 'price.adjusted',
                               - ref.date)
}


f.out <- paste0(my.name,'_', format.out, '_', first.date, '_', last.date,'.csv')
write.csv(x = df.tickers, file = f.out, row.names = F)

f.out <- paste0(my.name,'_', format.out, '_',first.date, '_', last.date,'.rds')
saveRDS(object = df.tickers, file = f.out)
#save('df.tickers',file = f.out)

write_csv(df.tickers, path = 'IbovStocks_long.csv')
