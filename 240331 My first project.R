

rm(list=ls())

library(ecos); 
library(fredr)
library(dplyr);
library(lubridate);
library(xts); 
library(ggfortify);
fredr_set_key("")

ecos.setKey('')

#—————————————————————–#
# Data
#—————————————————————–#

# 1. Uncollaterized Call Rate(Overnight)

call <- statSearch(stat_code = "721Y001",
                   item_code1 = "1010000", cycle="M")
call <- dplyr::select(call, time, data_value) %>% mutate(time=ym(time))

names(call) <- c("date","call")
call$date <- as.Date(call$date)
call <-xts(call$call, order.by=call$date)
colnames(call) <- "call"
# first differenced
diff_call <- diff(call, lag=12)
names(diff_call) <- c("diff_call")
autoplot(diff_call)

# 2. CPI, 2020=100

cpi <- statSearch(stat_code = "901Y009",
                  item_code1="0", cycle = "M")
cpi <- dplyr::select(cpi, time, data_value) %>% mutate(time=ym(time))

names(cpi) <- c("date", "cpi")
str(cpi)

cpi <- xts(cpi$cpi, order.by=cpi$date)
autoplot(cpi) +ggtitle("CPI")

# 12-month differenced
ln_cpi <- log(cpi)
diff_ln_cpi <- diff(ln_cpi, lag=12) * 100
names(diff_ln_cpi) <- c("diff_ln_cpi")
autoplot(diff_ln_cpi) + ggtitle("First Differenced Log-transformed CPI")


# 3. Industrial Production Index (nsa)

ind <- statSearch(stat_code = "901Y033",
                  item_code1 ="A00", item_code2="1", cycle="M") # item_code2 = "1" 원계열, 2 계절조정정

ind <- dplyr::select(ind, time, data_value) %>% mutate(time=ym(time))
names(ind) <- c("date", "ind")
str(ind)
ind <- xts(ind$ind, order.by=ind$date)
names(ind) <- c("ind")
autoplot(ind)+ ggtitle("전산업생산지수 계절조정")

ln_ind <- log(ind)
diff_ln_ind <- diff(ln_ind, lag=12) * 100
names(diff_ln_ind) <- c("diff_ln_ind")
autoplot(diff_ln_ind) + ggtitle("Differenced Log-transformed Industrial")


# 4. KRW/USD Exchange rate

exch <- statSearch(stat_code = "731Y006",
                   item_code1 ="0000003", item_code2="0000200", cycle="M") # item_code2 = "2" 말일자료, 1 평균자료

exch <- dplyr::select(exch, time, data_value) %>% mutate(time=ym(time))
names(exch) <- c("date", "exch")
str(exch)
exch <- xts(exch$exch, order.by=exch$date)
names(exch) <- c("exch")
autoplot(exch)+ ggtitle("원/달러(종가) 말일 자료")

ln_exch <- log(exch)
diff_ln_exch <- diff(ln_exch, lag=12) * 100
names(diff_ln_exch) <- c("diff_ln_exch")
autoplot(diff_ln_exch) + ggtitle("Differenced Log-transformed Industrial")

# 5. Brent Crude (nsa)
brent <- fredr(
  series_id = "POILBREUSDM",
  frequency = "m" # m: monthly, q: quarterly
)

brent <- dplyr::select(brent, date, value) 

names(brent) <- c("date", "brent")

str(brent)
brent <- xts(brent$brent, order.by=brent$date)
names(brent) <- c("brent")
brent <- na.omit(brent)
autoplot(brent)+ ggtitle("Global Price of Brent  Crude (nsa)")


ln_brent <- log(brent)
diff_ln_brent <- diff(ln_brent, lag=12) * 100
names(diff_ln_brent) <- c("diff_ln_brent")
autoplot(diff_ln_brent) + ggtitle("Differenced Log-transformed Brent")


