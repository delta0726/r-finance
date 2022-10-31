# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 5 Shape triangle of standardized GLD
# Date    : 2022/11/01
# Page    : P84 - P86
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 フィッテング
# 3 パラメータ抽出
# 4 プロット作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(FRAPO)
library(fBasics)


# データロード
data(INDTRACK3)


# 1 データ準備 ------------------------------------------------------------------

# リターン系列の作成
R <-
  INDTRACK3[, -1] %>%
    returnseries(method = "discret", trim = TRUE)


# 2 フィッテング ------------------------------------------------------------------

## Fitting and calculating beta and lambda
Fit <-
  R %>%
    apply(2, gldFit, method = "rob", doplot = FALSE, trace = FALSE)



# 3 パラメータ抽出 -----------------------------------------------------------------

# 関数定義
extract_params <- function(x){
  l <- x@fit$estimate[c(3, 4)]
  res <- c(l[2] - l[1], l[1] + l[2])
  res
}

# データ抽出
DeltaBetaParam <-
  Fit %>%
    lapply(extract_params) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE)


# 4 プロット作成 ---------------------------------------------------------------------

# 散布図
DeltaBetaParam %>%
  plot(xlim = c(-2, 2), ylim = c(-2, 0),
       xlab = expression(delta == lambda[4] - lambda[3]),
       ylab = expression(beta == lambda[3] + lambda[4]),
       pch = 19, cex = 0.5)

# コンポーネント
segments(x0 = -2, y0 = -2, x1 = 0, y1 = 0,
         col = "grey", lwd = 0.8, lty = 2)
segments(x0 = 2, y0 = -2, x1 = 0, y1 = 0,
         col = "grey", lwd = 0.8, lty = 2)
segments(x0 = 0, y0 = -2, x1 = 0, y1 = 0, col = "blue",
         lwd = 0.8, lty = 2)
segments(x0 = -0.5, y0 = -0.5, x1 = 0.5, y1 = -0.5,
         col = "red", lwd = 0.8, lty = 2)
segments(x0 = -1.0, y0 = -1.0, x1 = 1.0, y1 = -1.0,
         col = "red", lwd = 0.8, lty = 2)
