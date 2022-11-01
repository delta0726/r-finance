# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 1 Block maxima for Siemens losses
# Date    : 2022/11/02
# Page    : P103 - P107
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜参考＞
# - Rを使って極値理論をファイナンスへ応用してみた（その１）
# https://qiita.com/hrkz_szk/items/43debffda9697d9dd7a9


# ＜目次＞
# 0 準備
# 1 一般化極値分布によるモデリング
# 2 診断プロットの作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(evir)
library(ismev)
library(timeSeries)
library(fExtremes)


# データロード
data(siemens)

# データ確認
siemens %>% class()
siemens %>% as.data.frame() %>% head()

# プロット
SieDates <- as.character(format(as.POSIXct(attr(siemens, "times")), "%Y-%m-%d"))
SieRet <- timeSeries(siemens * 100, charvec = SieDates)
plot(SieRet, ylab="Log-return of Siemens")


# 1 一般化極値分布によるモデリング --------------------------------------

# ＜参考＞
# 生態学のデータ解析 - 極値分布
# https://kuboweb.github.io/-kubo/ce/ExtremeValue.html


# データ変換
# --- 小数をパーセントに変換して正負を逆転（損失を正の値として扱う）
SieLoss <- siemens * 100 * -1

# 一般化極値分布(GEV)に適合
# --- 半年間の最大値を自動的に抽出し、それらのデータをGEVに当てはめる
SieGEV <- SieLoss %>% gev(block = "semester")
SieGEV

# 最大値のプロット
SieGEV$data %>%
  plot(type = "h", col = "blue", xlab = "", ylab = "Block Maxima",
       main = "Maximum Biannual Losses of Siemens")


# 2 診断プロットの作成 -------------------------------------------------

SieGEV2 <- SieGEV$data %>% gev.fit(type = "GEV")
SieGEV2

# 診断データの作成
SieGEV2 %>% gev.diag()

# 診断プロットの作成
par(mfrow = c(2, 1))
SieGEV2 %>% gev.prof(m = 20, xlow = 5, xup = 16, conf = 0.95)
SieGEV2 %>% gev.profxi(xlow = 0.0, xup = 0.7, conf = 0.95)



#


mLoss <- SieGEV$data %>% max()
mYears <- 1 / (1 - pgev(mLoss, mu = SieGEV2$mle[1], beta = SieGEV2$mle[2], xi = SieGEV2$mle[3])) / 2
## package fExtremes:

SieGEV3 <-  SieGEV$data %>% gevFit(type = "pwm")
SieGEV3
