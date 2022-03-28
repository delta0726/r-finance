# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : managers.df
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/tree/master/R
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(managers.df)

# データ確認
managers.df %>% head()
managers.df %>% tail()
managers.df %>% names()
managers.df %>% dim()
