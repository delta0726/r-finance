# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 13 Tactical asset allocation
# Theme   : 6.4 Protection Strategy
# Date    : 2022/00/00
# Page    : P324 - P334
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 ローカルリターンの作成
# 3 予測リターンの作成
# 4 リスク指標の計算
# 5 バックテストの実行
# 6 パフォーマンス統計量


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(evir)
library(forecast)
library(PerformanceAnalytics)

# データロード
data(StockIndexAdjD)
data(ESCBFX)

# データ確認
StockIndexAdjD %>% head()
ESCBFX %>% head()

# データ期間
StockIndexAdjD %>% rownames() %>% as.Date() %>% summary()
ESCBFX %>% rownames() %>% as.Date() %>% summary()


# 1 データ準備 -----------------------------------------------------------

# 株価データ
PDaily <-
  StockIndexAdjD %>%
    timeSeries(charvec = rownames(StockIndexAdjD))

# FXデータ
FXDaily <-
  ESCBFX %>%
    timeSeries(charvec = rownames(ESCBFX)) %>%
    window(start = start(.), end = end(PDaily))

# 日付一覧
DDates <- FXDaily %>% time()

# 分析期間の設定
# --- 水曜日フラグ
# --- 開始日/終了日
WedDays <- DDates %>% isWeekday(wday = 3)
FirstWed <- WedDays %>% which(arr.ind = TRUE) %>% head(1)
LastWed <- WedDays %>% which(arr.ind = TRUE) %>% tail( 1)

# 週次カレンダーの作成
AllWedDays <-
  timeSequence(from = DDates[FirstWed],
               to = DDates[LastWed],
               by = "week")

# 週次データの作成
DumWed <- rep(1, length(AllWedDays)) %>% timeSeries(charvec = AllWedDays)
PWeekly <- DumWed %>% cbind(PDaily) %>% interpNA(method = "before") %>% .[AllWedDays, -1]
FXWeekly <- DumWed %>% cbind(FXDaily) %>% interpNA(method = "before") %>% .[AllWedDays, -1]

PWeekly %>% head()


# 2 ユーロ建てリターンの作成 --------------------------------------------------

# データコピー
PEWeekly <- PWeekly

# 株価変換
# --- ユーロ建てに変換
PEWeekly[, "SP500"] <- PWeekly[, "SP500"] / FXWeekly[, "USD"]
PEWeekly[, "N225"] <- PWeekly[, "N225"] / FXWeekly[, "JPY"]
PEWeekly[, "FTSE100"] <- PWeekly[, "FTSE100"] / FXWeekly[, "GBP"]
PEWeekly[, "HSI"] <- PWeekly[, "HSI"] / FXWeekly[, "HKD"]

# リターン計算
# --- 期間リターン
# --- 対数株価
REDWeekly <- (PEWeekly / lag(PEWeekly, k = 1) - 1)
PELWeekly <-  PEWeekly %>% log()

# プロット確認
REDWeekly %>% plot(main = "", xlab = "", col = "black")
PEWeekly %>% plot(main = "", xlab = "", col = "black")


# 3 予測リターンの作成 ---------------------------------------------------------

# 期間設定
# --- エンドポイント
# --- スタートポイント
epoints <- PELWeekly %>% time() %>% .[-c(1:259)]
size <- epoints %>% length()
spoints <- PELWeekly %>% time() %>% .[1:size]
idx <- 1:size

# パラメータ設定
NAssets <- PEWeekly %>% ncol()


# 関数定義
## Producing one-step ahead forecasts
Forecast <- function(x, order = c(1, 1, 1), ...){
  mod <- arima(x, order = order, ...)
  f1 <- forecast(mod, h = 1)$mean
  re <- (f1 - tail(x, 1)) * 100
  re
}

RetExp <- function(x, order = c(1, 1, 1), ...){
  ans <- apply(x, 2, Forecast, order = order, ...)
  ans
}

# 予測リターンの作成
RE <- PELWeekly %>% fapply(from = spoints, to = epoints, FUN = RetExp)

# データ確認
RE %>% head()
RE %>% tail()


# 4 リスク指標の計算 ----------------------------------------------------------

# リスク指標の計算
## Computing market risk measures
## Long/Short risk
RiskFac <- -1.0 * sign(RE)
ES <- matrix(0, ncol = NAssets, nrow = size)

# 関数定義
GpdEs <- function(x, nextremes = 30, method = "pwm", level = 0.95, RiskFac){
  x <- RiskFac * x
  mod <- gpd(data = x, nextremes = nextremes, method = method)
  GpdEs <- riskmeasures(mod, level)[3]
  GpdEs
}

# リスク指標の計算
for(i in idx){
    DatSub <- REDWeekly %>% window(start = spoints[i], end = epoints[i])
    FacSub <- RiskFac %>% window(start = epoints[i], end = epoints[i])
    for(j in 1:6){
      ES[i, j] <-
        DatSub[, j] %>%
          na.omit() %>%
          GpdEs(RiskFac = FacSub[, j])
    }
}


ES <-
  ES %>%
    timeSeries(charvec = epoints) %>%
    set_colnames(colnames(REDWeekly))

# データ確認
ES %>% head()
ES %>% tail()


# 5 バックテストの実行 -------------------------------------------------------

# 関数定義
# --- Creating LP
Lp <- function(RE, ES, Buffer, ub = 0.4){
  obj <- as.vector(RE)
  ## Initialise LHS matrix and RHS vector
  nvar <- length(obj)
  ## Wealth constraint
  a1 <- rep(1, nvar)
  b1 <- 1
  d1 <- "<="
  ## Risk constraint
  a2 <- as.vector(ES)
  b2 <- Buffer
  d2 <- "<="
  ## Upper bound
  a3 <- diag(nvar)
  b3 <- rep(ub, nvar)
  d3 <- rep("<=", nvar)
  ## Combining
  A <- rbind(a1, a2, a3)
  b <- c(b1, b2, b3)
  d <- c(d1, d2, d3)
  ans <- Rglpk_solve_LP(obj, mat = A, dir = d, rhs = b, max = TRUE)
  ans
}



# 格納用オブジェクト
LO <- rep(NA, size) %>% timeSeries(charvec = epoints)
LO[1] <- 100

# パラメータ設定
PLevel <- 0.9
FLO <- LO[1, ] * PLevel
Returns <- REDWeekly[epoints, ]
MoneyRate <- 1.01^(1/52) - 1


## Simulation
for(i in 2:size){
  BLO <- c(PLevel * LO[i - 1, ])
  if(BLO < FLO){
    LO[i, ] <- LO[i - 1, ] * (1 + MoneyRate)
  } else {
    re <- c(RE[i -1, ])
    if(all(re <= 0)){
      LO[i, ] <- LO[i - 1, ] * (1 + MoneyRate)
    } else {
      es <- c(ES[i -1, ])
      r <- c(Returns[i, ])
      B <- c(LO[i - 1, ]) / c(FLO) - 1
      ans <- Lp(RE = re, ES = es, Buffer = B, ub = 0.4)
      w <- ans$solution
      LO[i, ] <- LO[i - 1, ] * (1 + t(w) %*% c(r))
    }
  }
}


# Equal-weighted long-only strategy
EwRetfac <- 1 + rowMeans(Returns)
EwRetfac[1] <- 100
EW <- EwRetfac %>% cumprod() %>% timeSeries(epoints)

# プロット比較
ylims <- LO %>% cbind(EW) %>% range()
plot(LO, ylim = ylims, xlab = "", ylab = "Index")
lines(EW, col = "blue", lty = 2)
legend("topleft",
       legend = c("TAA long-only", "EW long-only"),
       lty = 1:2, col = c("black", "blue"))


# 6 パフォーマンス統計量 -----------------------------------------------

# ポートフォリオリターン
LORet <- LO %>% returns(method = "discrete", percentage = FALSE, trim = TRUE)
EWRet <- EW %>% returns(method = "discrete", percentage = FALSE, trim = TRUE)

# VaR
LOVAR <- LORet %>% VaR(, p = 0.95, method = "gaussian") %>% multiply_by(-100)
EWVAR <- EWRet %>% VaR(p = 0.95, method = "gaussian") %>% multiply_by(-100)

# ES
LOES <- LORet %>% ES(p = 0.95, method = "gaussian") %>% multiply_by(-100)
EWES <- EWRet %>% ES(p = 0.95, method = "gaussian") %>% multiply_by(-100)

# シャープレシオ
LOSR <- LORet %>% SharpeRatio()
EWSR <- EWRet %>% SharpeRatio()

# 年率化リターン
LORA <- LORet %>% Return.annualized(scale = 52)
EWRA <- EWRet %>% Return.annualized(scale = 52)
