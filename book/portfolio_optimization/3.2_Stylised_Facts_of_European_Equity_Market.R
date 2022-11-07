# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 3 Financial Market Data
# Theme   : 1.2 Stylized Fact for multivariate series
# Date    : 2022/11/08
# Page    : P32 - P35
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - ポートフォリオの観点からは多変量リターン系列の特性が重要
# - リターンシリーズ間の相関はそれほど明確でない（投資行動のインセンティブ）
# - ボラティリティ・クラスタリングは明確に確認できる


# ＜目次＞
# 0 準備
# 1 データ変換とプロット
# 2 リターン変換
# 3 Stylized Factの確認
# 4 ローリング相関の確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(zoo)


# データロード
data(EuStockMarkets)

# データ確認
# --- 株価指数の指数値データ
EuStockMarkets %>% head()
EuStockMarkets %>% class()
EuStockMarkets %>% glimpse()


# 1 データ変換とプロット ----------------------------------------------------------

# データ変換
# --- zooオブジェクトに変換
EuStockLevel <-
  EuStockMarkets %>%
    as.zoo() %>%
    .[, c("DAX", "CAC", "FTSE")]


# データ確認
EuStockLevel %>% head()

# プロット確認
EuStockLevel %>% plot(xlab = "", main = "")


# 2 リターン変換 ----------------------------------------------------------------

# リターンに変換
EuStockRet <-
  EuStockLevel %>%
    log() %>%
    diff() %>%
    multiply_by(100)

# プロット確認
EuStockRet %>% plot(xlab = "", main = "")


# 3 Stylized Factの確認 --------------------------------------------------------

# ＜ポイント＞
# - 左側がリターンのCCF、右側が絶対値リターンのCCF
# - 絶対値リターンで見ると時系列相関が共通して高いことが確認できる


# レイアウト設定
layout(matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE))


# DAX vs CAC ----------------------------------

ccf(EuStockRet$DAX, EuStockRet$CAC, ylab = "", xlab = "",
    lag.max = 20, main = "Returns DAX vs CAC")

ccf(abs(EuStockRet$DAX), abs(EuStockRet$CAC), ylab = "",
    xlab = "", lag.max = 20, main = "Absolute returns DAX vs CAC")


# DAX vs FTSE ---------------------------------

ccf(EuStockRet$DAX, EuStockRet$FTSE, ylab = "", xlab = "",
    lag.max = 20, main = "Returns DAX vs FTSE")

ccf(abs(EuStockRet$DAX), abs(EuStockRet$FTSE), ylab = "",
    xlab = "", lag.max = 20, main = "Absolute returns DAX vs FTSE")


# CAC vs FTSE ---------------------------------

ccf(EuStockRet$CAC, EuStockRet$FTSE, ylab = "", xlab = "",
    lag.max = 20, main = "Returns CAC vs FTSE")

ccf(abs(EuStockRet$CAC), abs(EuStockRet$FTSE), ylab = "",
    xlab = "", lag.max = 20, main = "Absolute returns CAC vs FTSE")


# 4 ローリング相関の確認 ---------------------------------------------------------

# ＜ポイント＞
# - 各系列は相関の水準や推移の点でかなり類似している


# 関数定義
# --- ローリング相関
rollc <- function(x){
  dim <- ncol(x)
  rcor <- cor(x)[lower.tri(diag(dim), diag = FALSE)]
  return(rcor)
}

# ローリング相関の計算
rcor <-
  EuStockRet %>%
    rollapply(width = 250, rollc,
              align = "right", by.column = FALSE) %>%
    set_colnames(c("DAX & CAC", "DAX & FTSE", "CAC & FTSE"))

# プロット作成
rcor %>% plot(main = "", xlab = "")
