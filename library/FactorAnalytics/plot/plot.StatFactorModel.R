# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : plot.FM.attribution
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/blob/master/R/plot.StatFactorModel.r
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(stat.fm.data)

# データ確認
sfm.dat %>% head()
sfm.dat %>% tail()
sfm.dat %>% dim()

# モデル構築
sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat, k = 10)

# モデル出力
sfm.pca.fit %>% print()
sfm.pca.fit %>% names()

# プロット確認
# --- 1 = "Screeplot of Eigenvalues",
# --- 2 = "Factor returns",
# --- 3 = "FM Correlation",
# --- 4 = "R square",
# --- 5 = "Variance of Residuals",
# --- 6 = "Factor Contributions to SD",
# --- 7 = "Factor Contributions to ES",
# --- 8 = "Factor Contributions to VaR"
sfm.pca.fit %>% plot(which.plot  = 1)
sfm.pca.fit %>% plot(which.plot  = 2)
sfm.pca.fit %>% plot(which.plot  = 3)
sfm.pca.fit %>% plot(which.plot  = 4)
sfm.pca.fit %>% plot(which.plot  = 5)
sfm.pca.fit %>% plot(which.plot  = 6)
sfm.pca.fit %>% plot(which.plot  = 7)
sfm.pca.fit %>% plot(which.plot  = 8)

data(stat.fm.data)
# pca
sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
args(plot.StatFactorModel)
# plot all
plot(sfm.pca.fit)
# plot single asset