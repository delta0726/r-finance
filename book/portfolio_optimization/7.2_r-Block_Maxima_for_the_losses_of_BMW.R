# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 2 r-block maxima for BMW losses
# Date    : 2022/11/02
# Page    : P107 - P110
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 最大損失データの抽出
# 2 最大損失のプロット
# 3 フィッティングとモデル診断


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(timeSeries)
library(evir)
library(ismev)


# データロード
data(bmw)

# データ確認
bmw %>% class()
bmw %>% as.data.frame() %>% head()

# プロット
BmwDates <- as.character(format(as.POSIXct(attr(bmw, "times")), "%Y-%m-%d"))
BmwRet <- timeSeries(bmw * 100, charvec = BmwDates)
BmwRet %>% plot(ylab="Log-return of BMW")


# 1 最大損失データの抽出 ---------------------------------------------------------------

# データ変換
# --- 小数をパーセントに変換して正負を逆転（損失を正の値として扱う）
BmwLoss <- bmw * 100 * -1.0

# パーツ作成
Yearu <- BmwLoss %>% attr("time") %>% format( "%Y") %>% unique()
idx <- 1:length(Yearu)

# 関数定義
# --- 最大損失の上位2つを抽出
test_f <- function(x){
  BmwLoss[attr(BmwLoss, "years") == Yearu[x]] %>%
    sort(decreasing = TRUE) %>%
    head(2)
}

# データ作成
BmwOrder <-
  idx %>%
    sapply(test_f) %>%
    t() %>%
    set_colnames(c("r1", "r2")) %>%
    set_rownames(Yearu)


# 2 最大損失のプロット -----------------------------------------------------------------

Yearu %>%
  plot(BmwOrder[, 1], col = "black", ylim = range(BmwOrder),
       ylab = "Losses BMW (percentages)", xlab = "",
       pch = 21, bg = "black")

Yearu %>%
  points(BmwOrder[, 2], col = "grey", pch = 23, bg = "grey")


# 3 フィッティングとモデル診断 ---------------------------------------------------------

# フィッティング
BmwOrderFit <- BmwOrder %>% rlarg.fit()

# 確認
BmwOrderFit %>% print()

# モデル診断
BmwOrderFit %>% rlarg.diag()
