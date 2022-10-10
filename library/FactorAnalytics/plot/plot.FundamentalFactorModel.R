# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : plot.FM.attribution
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/blob/master/R/plot.FundamentalFactorModel.r
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(Stock.df)

# データ確認
stock %>% glimpse()
stock %>% head()
stock %>% tail()
stock %>% dim()


# モデル構築
fit.fund <-
  fitFundamentalFactorModel(data = data,
                            exposure.names = c("BOOK2MARKET", "LOG.MARKETCAP"),
                            datevar = "DATE",
                            returnsvar = "RETURN",
                            assetvar = "TICKER",
                            wls = TRUE,
                            regression = "classic",
                            covariance = "classic",
                            full.resid.cov = TRUE,
                            robust.scale = TRUE)

plot(fit.fund)