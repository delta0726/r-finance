# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 3 Financial Market Data
# Theme   : 1.1 Stylized Facts for univariate series
# Date    : 2022/10/29
# Page    : P29 - P32
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - 金融市場データの典型的な特徴は｢Stylized Facts｣として知られている
# - 金融市場データの時系列特性を適切に捉えていないリスクモデルはリスク指標の導出には役に立たない


# ＜Stylized Facts＞
# - 日次リターンは一般に独立しておらず同一分布(iid)となっている
# - ボラティリティは時間に対して一定ではない
# - 絶対リターンまたは二乗リターンは高度に自己相関している（ボラティリティの継続）
# - リターン分布は正規分布よりも尖度が高い（極端なリターンが発生しやすい）
# - 極端なリターンは、時間的に密接に観察されます (ボラティリティ クラスタリング)
# - リターンの経験的分布は左に歪んでいる（マイナスリターンはプラスリターンよりも発生する可能性が高い）


# ＜目次＞
# 0 準備
# 1 リターンデータの作成
# 2 Stylised Facts I
# 3 Stylised Facts II


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(timeSeries)
library(fBasics)
library(evir)

# データロード
data(siemens)


# 1 リターンデータの作成 --------------------------------------------------------

# ＜ポイント＞
# - シーメンス社の株価リターンでStylized Factを確認する（1973/1/2 to 1996/7/23）



SieDates <-
  siemens %>%
    attr("times") %>%
    as.POSIXct() %>%
    format("%Y-%m-%d") %>%
    as.character()

SieRet <-
  siemens %>%
    multiply_by(100) %>%
    timeSeries(charvec = SieDates) %>%
    set_colnames("SieRet")

# データ確認
SieRet %>% class()
SieRet %>% glimpse()
SieRet %>% head()


# 2 Stylised Facts I ------------------------------------------------------------

# ＜ポイント＞
# - 日次リターン系列からStylized Factを確認する


# プロット分割
par(mfrow = c(2, 2))

# 日次リターン
# --- ボラティリティは時間に対して一定ではない（ボラティリティ・クラスタリングが発生）
SieRet %>%
  seriesPlot(title = FALSE, main = "Daily Returns of Siemens", col = "blue")

# ボックスプロット
# --- リターン分布は正規分布よりも尖度が高い（極端なリターンが発生しやすい）
# --- リターンの経験的分布は左に歪んでいる（マイナスリターンはプラスリターンよりも発生する可能性が高い）
SieRet %>%
  boxPlot(title = FALSE, main = "Box plot of Returns",
          col = "blue", cex = 0.5, pch = 19)

# ACFプロット
# --- 日次リターンは一般に独立しておらず同一分布(iid)となっている
SieRet %>%
  acf(main = "ACF of Returns", lag.max = 20, ylab = "", xlab = "",
      col = "blue", ci.col = "red")

# PACFプロット
SieRet %>%
  pacf(main = "PACF of Returns", lag.max = 20, ylab = "", xlab = "",
       col = "blue", ci.col = "red")


# 3 Stylised Facts II ------------------------------------------------------------

# ＜ポイント＞
# - 絶対値リターンで検証することで、ボラティリティ・クラスタリングを明示的に示す
# - 極端なリターンにフォーカスを当てて、ボラティリティ・クラスタリングを明示的に示す


# 絶対値リターン
SieRetAbs <- SieRet %>% abs()

# ハードルリターン
# --- 絶対値リターンの大きいほうから100番目
SieRet100 <- SieRet %>% series() %>% abs() %>% sort() %>% tail(100) %>% head(1)

# ハードルを超えるリターン系列
# --- ハードルリターン以下をゼロとする
SieRetAbs100 <-
  SieRetAbs %>%
    series() %>%
    as.data.frame() %>%
    rownames_to_column(var = "date") %>%
    mutate(SieRet = ifelse(SieRet < SieRet100, 0, SieRet)) %>%
    use_series(SieRet) %>%
    timeSeries(charvec = SieDates)


# プロット分割
par(mfrow = c(2, 2))

# 自己相関(ACF)
# --- 絶対値リターンは自己相関を持つ
SieRetAbs %>%
  acf(main = "ACF of Absolute Returns", lag.max = 20,
      ylab = "", xlab = "", col = "blue", ci.col = "red")

# 自己相関(PACF)
# --- 絶対値リターンは自己相関を持つ
SieRetAbs %>%
  pacf(main = "PACF of Absolute Returns", lag.max = 20,
       ylab = "", xlab = "", col = "blue", ci.col = "red")

# QQ-Plot
# --- マイナスリターンの方が裾が長い（-10 to +5）
SieRet %>%
  qqnormPlot(main = "QQ-Plot of Returns", title = FALSE,
             col = "blue", cex = 0.5, pch = 19)

# ボラティリティ・クラスタリング
# --- ボラティリティの高い期間は継続する（ボラティリティは自己相関を持つ）
SieRetAbs100 %>%
  plot(type = "h", main = "Volatility Clustering",
       ylab = "", xlab = "", col = "blue")
