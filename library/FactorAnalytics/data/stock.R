# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : Stock.df
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/tree/master/R
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(Stock.df)

# データ確認
stock %>% head()
stock %>% tail()
stock %>% names()
stock %>% dim()
