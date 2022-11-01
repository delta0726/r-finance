# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 1
# Date    : 2022/00/00
# Page    : P - P
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 損失が大きいレコードの抽出
# 3 クラスタリング


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(timeSeries)
library(fExtremes)
library(fBasics)

# データロード
data(nyse)

# データ確認
nyse %>% as_tibble()


# 1 データ準備 -----------------------------------------------------------------

# データ変換
NYSELevel <-
  nyse %$%
    timeSeries(NYSE, charvec = as.character(X.Y..m..d.))

# リターン
# --- 損失計算のため符号逆転
# --- パーセント表示
NYSELoss <-
  NYSELevel %>%
    log() %>%
    diff() %>%
    multiply_by(-100) %>%
    na.omit() %>%
    set_colnames("NYSELoss")

# データ確認
NYSELevel %>% head()
NYSELoss %>% head()

# レコード数
NYSELevel %>% length()
NYSELoss %>% length()


# 2 損失が大きいレコードの抽出 ----------------------------------------------------

# データ抽出
# --- 95パーセンタイルを超えるレコード
NYSEPP <- NYSELoss %>% pointProcess(u = quantile(NYSELoss, 0.95))

# 確認
NYSEPP %>% head()
NYSELoss[getTime(head(NYSEPP)), ]


# 3 クラスタリング --------------------------------------------------------------

## Declustering
DC05 <- NYSEPP %>% deCluster(run = 5, doplot = FALSE)
DC10 <- NYSEPP %>% deCluster(run = 10, doplot = FALSE)
DC20 <- NYSEPP %>% deCluster(run = 20, doplot = FALSE)
DC40 <- NYSEPP %>% deCluster(run = 40, doplot = FALSE)
DC60 <- NYSEPP %>% deCluster(run = 60, doplot = FALSE)
DC120 <- NYSEPP %>% deCluster(run = 120, doplot = FALSE)

## Fit of declustered data
DC05Fit <- DC05 %>% gpdFit(u = min(.))
DC10Fit <- DC10 %>% gpdFit(u = min(.))
DC20Fit <- DC20 %>% gpdFit(u = min(.))
DC40Fit <- DC40 %>% gpdFit(u = min(.))
DC60Fit <- DC60 %>% gpdFit(u = min(.))
DC120Fit <- DC120 %>% gpdFit(u = min(.))
