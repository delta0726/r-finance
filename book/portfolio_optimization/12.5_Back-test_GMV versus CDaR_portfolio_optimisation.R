# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 12 Risk-Optimal Portfolio
# Theme   : 5 Backtest comparison for stock portfolio
# Date    : 2022/11/04
# Page    : P260 - P265
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 バックテスト
# 3 バックテスト結果の整理
# 4 ポートフォリオリターンの計算
# 5 ドローダウンの比較
# 6 その他のパフォーマンス統計量


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(fPortfolio)
library(PerformanceAnalytics)

# データロード
data(EuroStoxx50)

# データ確認
# --- 個別銘柄データ
EuroStoxx50 %>% head()
EuroStoxx50 %>% dim()


# 1 データ準備 ---------------------------------------------------------------

# 株価データ
pr <- EuroStoxx50 %>% timeSeries(charvec = rownames(EuroStoxx50))

# アセット数
NAssets <- pr %>% ncol()

# リターン作成
RDP <- na.omit((pr / lag(pr, k = 1) - 1) * 100)


# 2 バックテスト -------------------------------------------------------------

# 期間設定
# --- 開始日は固定
to <- RDP %>% time() %>% .[208:nrow(RDP)]
from <- RDP %>% start() %>% rep(length(to))

# 格納用オブジェクト
wMV <- matrix(NA, ncol = ncol(RDP), nrow = length(to))
wCD <- matrix(NA, ncol = ncol(RDP), nrow = length(to))


## Conducting backtest
i <- 1
for(i in seq_along(to)){

  mv <-
    RDP %>%
      window(start = from[i], end = to[i]) %>%
      minvariancePortfolio(spec = portfolioSpec(),
                           constraints =  c("minsumW[1:NAssets] = 0.0", "maxsumW[1:NAssets] = 1.0"))

  cd <-
    pr %>%
      window(start = from[i], end = to[i]) %>%
      PCDaR(alpha = 0.10,
            bound = 0.95,
            softBudget = TRUE)

  # ウエイト取得
  wMV[i, ] <- mv %>% getWeights()
  wCD[i, ] <- cd %>% Weights()
}


# 3 バックテスト結果の整理 ------------------------------------------------------

# ウエイトデータ
wMVL1 <-
  rep(NA, ncol(RDP)) %>%
    rbind(wMV[-nrow(wMV), ]) %>%
    timeSeries(charvec = to) %>%
    set_colnames(names(RDP))

# ウエイトデータ
wCDL1 <-
  rep(NA, ncol(RDP)) %>%
    rbind(wCD[-nrow(wCD), ]) %>%
    timeSeries(charvec = to) %>%
    set_colnames(names(RDP))

# リターン
RDPback <- RDP %>% .[to,] %>% set_colnames(names(RDP))

# データ確認
wMVL1 %>% head()
wCDL1 %>% head()
RDPback %>% head()


# 4 ポートフォリオリターンの計算 ----------------------------------------------------

# ポートフォリオリターン
MVRetFac <- 1 + rowSums(wMVL1 * RDPback) / 100
CDRetFac <- 1 + rowSums(wCDL1 * RDPback) / 100

# ベース値の挿入
MVRetFac[1] <- 100
CDRetFac[1] <- 100

# 累積リターンの計算
MVPort <- MVRetFac %>% cumprod() %>% timeSeries(charvec = names(.))
CDPort <- CDRetFac %>% cumprod() %>% timeSeries(charvec = names(.))

# 期間リターンの計算
MVRet <- MVPort %>% returns(method = "discrete", percentage = FALSE, trim = TRUE)
CDRet <- CDPort %>% returns(method = "discrete", percentage = FALSE, trim = TRUE)

# プロット作成
ylims <- range(cbind(MVPort, CDPort))
plot(CDPort, main = "", ylim = ylims, ylab = "Index values",
     xlab = "")
lines(MVPort, col = "darkgrey")
legend("topleft", legend = c("CDaR", "GMV"),
       col = c("black", "darkgrey"),
       lty = 1)


# 5 ドローダウンの比較 ----------------------------------------------------------

# ドローダウン・ランキング
MVRet %>% table.Drawdowns()
CDRet %>% table.Drawdowns()

# ドローダウン
MVD <- MVRet %>% Drawdowns() %>% multiply_by(100)
CDD <- CDRet %>% Drawdowns() %>% multiply_by(100)

# プロット作成
plot(CDD, main = "", ylab = "Percentages", xlab = "",
     ylim = c(min(c(MVD, CDD)), 0))
lines(MVD, col = "darkgrey")
abline(h = 0, col = "lightgrey")
abline(h = -10, col = "lightgrey", lty = 2)
legend("bottomleft", legend = c("CDaR", "GMV"),
       col = c("black", "darkgrey"), lty = 1)


# 6 その他のパフォーマンス統計量 ------------------------------------------------

# VaR
MVVAR <- MVRet %>% VaR(p = 0.95, method = "gaussian") %>% multiply_by(-100)
CDVAR <- CDRet %>% VaR(p = 0.95, method = "gaussian") %>% multiply_by(-100)

# ES
MVES <- MVRet %>% ES(p = 0.95, method = "gaussian") %>% multiply_by(-100)
CDES <- CDRet %>% ES(p = 0.95, method = "gaussian") %>% multiply_by(-100)

# Sharpe
MVSR <- MVRet %>% SharpeRatio()
CDSR <- CDRet %>% SharpeRatio()

# Annualised returns
MVRA <- MVRet %>% Return.annualized(scale = 52)
CDRA <- CDRet %>% Return.annualized(scale = 52)

# Draw downs
MVDD <- MVRet %>% findDrawdowns() %>% use_series(return) %>% multiply_by(-100)
MVDD <- MVDD[MVDD!=0.0]
MVDD %>% length()
MVDD %>% summary()

CDDD <- CDRet %>% findDrawdowns() %>% use_series(return) %>% multiply_by(-100)
CDDD <- CDDD[CDDD!=0.0]
length(CDDD)
summary(CDDD)
