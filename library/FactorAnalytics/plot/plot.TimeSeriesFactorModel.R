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
fit.macro <-
  fitTimeseriesFactorModel(assets.names = colnames(managers.df[,(1:6)]),
                           factors.names = c("EDHEC.LS.EQ","SP500.TR"),
                           data = managers.df,
                           fit.method = "OLS")

# モデル出力
sfm.pca.fit %>% print()
sfm.pca.fit %>% names()

# プロット確認
# --- 1 = "Fitted factor returns",
# --- 2 = "R square",
# --- 3 = "Variance of Residuals",
# --- 4 = "FM Correlation",
# --- 5 = "Factor Contributions to SD",
# --- 6 = "Factor Contributions to ES",
# --- 7 = "Factor Contributions to VaR"
sfm.pca.fit %>% plot(which.plot  = 1)
sfm.pca.fit %>% plot(which.plot  = 2)
sfm.pca.fit %>% plot(which.plot  = 3)
sfm.pca.fit %>% plot(which.plot  = 4)
sfm.pca.fit %>% plot(which.plot  = 5)
sfm.pca.fit %>% plot(which.plot  = 6)
sfm.pca.fit %>% plot(which.plot  = 7)
sfm.pca.fit %>% plot(which.plot  = 8)
