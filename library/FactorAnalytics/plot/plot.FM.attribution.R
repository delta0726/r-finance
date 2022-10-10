# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : plot.FM.attribution
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/blob/master/R/plot.FM.attribution.r
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(managers.df)

# データ確認
managers.df %>% head()
managers.df %>% tail()
managers.df %>% dim()

# モデル構築
fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                  factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                  data=managers.df,fit.method="OLS")

# モデル出力
sfm.pca.fit %>% print()
sfm.pca.fit %>% names()

# 要因分析
fm.attr <- fit.ts %>% factorModelPerformanceAttribution()
fm.attr %>% print()

# プロット確認
# --- 1 = attributed cumulative returns,
# --- 2 = attributed returns on date selected by user,
# --- 3 = time series of attributed returns
fm.attr %>% plot(legend.loc = "topleft", which.plot  = 1)
fm.attr %>% plot(legend.loc = "topleft", which.plot  = 2)
fm.attr %>% plot(legend.loc = "topleft", which.plot  = 3)
