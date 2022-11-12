# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 6.3 Stylized Facts revisited
# Date    : 2022/11/12
# Page    : P80 - P82
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - リターン期間ごとのリターン分布の尖度の違いを確認する
# - HWP社の株価リターンからStylized Factの過剰尖度を確認する
#   --- 通常、金融市場のリターンの経験的分布は｢過剰尖度｣と｢負の歪度｣によって特徴付けられる


# ＜目次＞
# 0 準備
# 1 リターン系列の作成
# 2 双曲型分布(HYP)によるフィッティング
# 3 密度プロットの比較
# 4 パラメータの抽出
# 5 Shape Triangleの作成


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(conflicted)
library(ghyp)
library(timeSeries)
library(fBasics)
library(Hmisc)

# コンフリクト解消
conflict_prefer("select", "dplyr")


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
  DowJones30_Mod %>%
    select(Period, HWP) %$%
    timeSeries(data = HWP, charvec = Period)

# リターン期間
rd <- c(1, 5, 10, 20, 40)

# ローリングリターンの作成
# --- 列ごとにリターン期間が異なる
yrets <-
  rd %>%
    lapply(function(x) diff(log(y), lag = x)) %>%
    unlist() %>%
    matrix(ncol = 5) %>%
    na.omit()


# 2 双曲型分布(HYP)によるフィッティング ---------------------------------------------------

# ＜ポイント＞
# - 列ごとに分布モデルを構築する
#   --- 列ごとに異なる期間リターン


# HYP Fitting
hypfits <- yrets %>% apply( 2, fit.hypuv, symmetric = FALSE)

# 確認
hypfits %>% list.tree(2)
hypfits %>% glimpse()


# 3 密度プロットの比較 -----------------------------------------------------------------

# ＜ポイント＞
# - 双曲型分布(HYP)により経験的分布は概ね再現されている
# - リターン期間が短いほど過剰尖度が顕著となる


# カーネル密度
# --- HP社のリターン
ef <- yrets %>% apply(2, density)

# xの対応値の取得
y_pred_1 <- ef[[1]]$x %>% dghyp(hypfits[[1]])
y_pred_2 <- ef[[2]]$x %>% dghyp(hypfits[[2]])
y_pred_3 <- ef[[3]]$x %>% dghyp(hypfits[[3]])
y_pred_4 <- ef[[4]]$x %>% dghyp(hypfits[[4]])
y_pred_5 <- ef[[5]]$x %>% dghyp(hypfits[[5]])

# データ結合
X_Plot <-
  tibble(horizon = "01D", type = "Empirical", x = ef[[1]]$x, y = ef[[1]]$y) %>%
    bind_rows(tibble(horizon = "05D", type = "Empirical", x = ef[[2]]$x, y = ef[[2]]$y)) %>%
    bind_rows(tibble(horizon = "10D", type = "Empirical", x = ef[[3]]$x, y = ef[[3]]$y)) %>%
    bind_rows(tibble(horizon = "20D", type = "Empirical", x = ef[[4]]$x, y = ef[[4]]$y)) %>%
    bind_rows(tibble(horizon = "40D", type = "Empirical", x = ef[[5]]$x, y = ef[[5]]$y)) %>%
    bind_rows(tibble(horizon = "01D", type = "Est_HYP", x = ef[[1]]$x, y = y_pred_1)) %>%
    bind_rows(tibble(horizon = "05D", type = "Est_HYP", x = ef[[2]]$x, y = y_pred_2)) %>%
    bind_rows(tibble(horizon = "10D", type = "Est_HYP", x = ef[[3]]$x, y = y_pred_3)) %>%
    bind_rows(tibble(horizon = "20D", type = "Est_HYP", x = ef[[4]]$x, y = y_pred_4)) %>%
    bind_rows(tibble(horizon = "40D", type = "Est_HYP", x = ef[[5]]$x, y = y_pred_5))

# プロット作成
X_Plot %>%
  ggplot(aes(x = x, y = y, color = type, group = type)) +
  geom_line() +
  facet_wrap(~horizon, nrow = 2)


# 4 パラメータの抽出 ----------------------------------------------------------------------

# ＜ポイント＞
# - HYPを空間でパラメータ化する


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

# パラメータ取得
points <-
  hypfits %>%
    lapply(xichi) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE)


# 5 Shape Triangleの作成 -----------------------------------------------------------------

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
