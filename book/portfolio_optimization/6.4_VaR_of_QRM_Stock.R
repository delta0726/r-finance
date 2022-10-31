# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 4 VaR for a Single Stock
# Date    : 2022/11/01
# Page    : P82 - P84
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 ローリングでVaRの計算
# 3 データ整理
# 4 プロット作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(lmomco)
library(FRAPO)


# データロード
data(SP500)


# 1 データ準備 -----------------------------------------------------------------

# リターン系列の作成
L <-
  SP500$QCOM %>%
    returnseries(method = "discrete", trim = TRUE) %>%
    multiply_by(-1)


# 2 ローリングでVaRの計算 -------------------------------------------------------

## Computing VaR (Normal & GLD) 99%, moving window

# パラメータ設定
# --- 信頼区間
# --- 開始点
# --- 終了点
level <- 0.99
sp <- 1:length(ep)
ep <- 104:length(L)

# 格納用オブジェクト
VaR <- matrix(NA, ncol = 2, nrow = length(ep))

# VaRの計算
i <- 1
for(i in 1:length(sp)){
  x <- L[sp[i]:ep[i]]
  lmom <- lmom.ub(x)
  fit <- pargld(lmom)
  VaRGld <- quagld(level, fit)
  VaRNor <- qnorm(level, mean(x), sd(x))
  VaR[i, ] <- c(VaRGld, VaRNor)
  print(paste("Result for", ep[i], ":", VaRGld, "and", VaRNor))
}


# 3 データ整理 ----------------------------------------------------------------

# プロット用データ作成
Res <-
  L[105:length(L)] %>%
    cbind(VaR[-nrow(VaR), ]) %>%
    set_colnames(c("Loss", "VaRGld", "VaRNor"))


# 4 プロット作成 ---------------------------------------------------------------

# 散布図
plot(Res[, "Loss"], type = "p", xlab = "Time Index",
     ylab = "Losses in percent", pch = 19, cex = 0.5,
     ylim = c(-15, max(Res)))

# 原点
abline(h = 0, col = "grey")

# ラインチャート
lines(Res[, "VaRGld"], col = "blue", lwd = 2)
lines(Res[, "VaRNor"], col = "red", lwd = 2)

# 凡例追加
legend("bottomleft", legend = c("Losses", "VaR GLD", "VaR Normal"),
       col = c("black", "blue", "red"),
       lty = c(NA, 1, 1), pch = c(19, NA, NA), bty = "n")
