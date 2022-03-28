# ***********************************************************************************************]
# Library   : factorAnalytics
# Function  : CommonFactors
# Created on: 2022/03/29
# URL       : https://github.com/R-Finance/FactorAnalytics/tree/master/R
# ***********************************************************************************************


# ライブラリ
library(tidyverse)
library(factorAnalytics)

# データロード
data(CommonFactors)

# データ確認
factors %>% head()
factors %>% tail()
factors %>% names()
factors %>% dim()
