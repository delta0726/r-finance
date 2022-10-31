# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 3 Shape triangle for HYP distribution
# Date    : 2022/11/01
# Page    : P80 - P82
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 リターン系列の作成
# 2 HYPによるフィッティング
# 3 パラメータの抽出
# 4 Shape Triangleの作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(ghyp)
library(timeSeries)
library(fBasics)
library(Hmisc)


# データロード
data(DowJones30)

# データ確認
DowJones30 %>% class()
DowJones30 %>% glimpse()


# 1 リターン系列の作成 ----------------------------------------------------------

# データ加工
# --- Period列を作成
DowJones30_Mod <-
  DowJones30 %>%
    mutate(Period = as.character(X.Y..m..d)) %>%
    as_tibble()

# 日次株価の作成
# --- HP社のみを抽出
y <-
  DowJones30_Mod$HWP %>%
    timeSeries(charvec = DowJones30_Mod$Period)

# リターン期間
rd <- c(1, 5, 10, 20, 40)

# 期間リターンの作成
yrets <-
  lapply(rd, function(x) diff(log(y), lag = x)) %>%
    unlist() %>%
    matrix(ncol = 5) %>%
    na.omit()


# 2 HYPによるフィッティング --------------------------------------------------------------

# HYP Fitting
hypfits <- yrets %>% apply( 2, fit.hypuv, symmetric = FALSE)

# 確認
hypfits %>% glimpse()
hypfits %>% list.tree(2)


# 3 パラメータの抽出 ----------------------------------------------------------------------

# 関数定義
xichi <- function(x){
  param <- coef(x, type = "alpha.delta")
  rho <- param[["beta"]] / param[["alpha"]]
  zeta <- param[["delta"]] * sqrt(param[["alpha"]]^2 - param[["beta"]]^2)
  xi <- 1 / sqrt(1 + zeta)
  chi <- xi * rho
  result <- c(chi, xi)
  names(result) <- c("chi", "xi")
  return(result)
}


points <-
  hypfits %>%
    lapply(xichi) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE)


# 4 Shape Triangleの作成 -----------------------------------------------------------------

# パラメータ設定
col.def <- c("black", "blue", "red", "green", "orange")
leg.def <- rd %>% paste(rep("day return", 5))

# プロット作成
plot(points, ylim = c(-0.2, 1.2), xlim = c(-1.2, 1.2),
     col = col.def, pch = 16, ylab = expression(xi),
     xlab = expression(chi))
lines(x = c(0, -1), y = c(0, 1))
lines(x = c(0, 1), y = c(0, 1))
lines(x = c(-1, 1), y = c(1, 1))
legend("bottomright", legend = leg.def, col = col.def, pch = 16)
text(x = 0.0, y = 1.05, label = "Laplace", srt = 0)
text(x = -1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 1.0, y = 1.05, label = "Exponential", srt = 0)
text(x = 0.0, y = -0.1, label = "Normal", srt = 0)
text(x = -0.6, y = 0.5, label = "Hyperbolic, left skewed",
     srt = 302)
text(x = 0.6, y = 0.5, label = "Hyperbolic, right skewed",
     srt = 57)
