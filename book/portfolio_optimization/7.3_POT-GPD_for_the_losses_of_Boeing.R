# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 7 Extreme value theory
# Theme   : 3 POT GPD for Boeing Losses
# Date    : 2022/11/02
# Page    : P110 - P113
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 プロットによる確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(fBasics)
library(fExtremes)


# データロード
data(DowJones30)

# データ確認
DowJones30 %>% as_tibble()


# 1 データ準備 --------------------------------------------------------------

# データ変換
# --- timeSeriesオブジェクトに変換
DJ <-
  DowJones30[, -1] %>%
    timeSeries(charvec = as.character(DowJones30[, 1]))

# データ確認
DJ %>% class()

# BA社のリターン抽出
# --- 符号を逆転
BALoss <-
  DJ[, "BA"] %>%
    returns(percentage = TRUE, trim = TRUE) %>%
    multiply_by(-1.0)


# 2 プロットによる確認 ----------------------------------------------------------

## MRL-plot
BALoss %>% mrlPlot(umin = -10, umax = 10)

## GPD
BAFit <- BALoss %>% gpdFit(u = 3)

## Diagnostic plots
plot(BAFit)

## Risk measures
BAFit %>% gpdRiskMeasures(prob = c(0.95, 0.99, 0.995))
