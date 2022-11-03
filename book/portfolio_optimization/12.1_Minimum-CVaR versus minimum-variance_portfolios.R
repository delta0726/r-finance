# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 12 Risk-Optimal Portfolio
# Theme   : 1 Minimum-CVaR versus minimum-variance portfolios
# Date    : 2022/11/04
# Page    : P251 - P254
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 ポートフォリオ設定
# 3 バックテスト
# 4 ポートフォリオリターンの計算
# 5 プロット比較


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(fPortfolio)

# データロード
data(StockIndex)

# データ確認
StockIndex %>% head()


# 1 データ準備 -----------------------------------------------------------------

# リターン作成
StockReturn <-
  StockIndex %>%
    returnseries(method = "discrete") %>%
    timeSeries(charvec = rownames(.)) %>%
    na.omit()


# 2 ポートフォリオ設定 -----------------------------------------------------------


## Specifying portfolio
pspec <- portfolioSpec()
gmv <-  pspec
cvar <- pspec
setType(cvar) <- "CVaR"
setAlpha(cvar) <- 0.1
setSolver(cvar) <- "solveRglpk.CVAR"


# 3 バックテスト --------------------------------------------------------------

# 期間設定
end <- StockReturn %>% time() %>% .[60:239]
from <- StockReturn %>% time() %>% .[1:length(end)]

# 格納用オブジェクト
wGMV <-
  matrix(NA, ncol = ncol(StockReturn), nrow = length(end)) %>%
    set_colnames(names(StockReturn))

wCVAR <-
  matrix(NA, ncol = ncol(StockReturn), nrow = length(end)) %>%
    set_colnames(names(StockReturn))

# バックテスト
i <- 1
for(i in 1:length(end)){
  # Min-Vol
  wGMV[i, ] <-
    StockReturn %>%
      window(start = from[i], end = end[i]) %>%
      minvariancePortfolio(spec = gmv, constraints = "LongOnly") %>%
      getWeights()

  # CVaR
  wCVAR[i, ] <-
    StockReturn %>%
      window(start = from[i], end = end[i]) %>%
      minvariancePortfolio(spec = cvar, constraints = "LongOnly") %>%
      getWeights()
}

# データ変換
# --- Min-Vol
# --- CVaR
wGMVL1 <- wGMV %>% timeSeries(charvec = end) %>% lag(k = 1)
wCVAR1 <- wCVAR %>% timeSeries(charvec = end) %>% lag(k = 1)

# データ確認
wGMVL1 %>% head()
wCVAR1 %>% head()


# 4 ポートフォリオリターンの計算 -----------------------------------------------

# 寄与度
# --- Min-Vol
# --- CVaR
GMVRetFac <- 1 + rowSums(wGMVL1 * StockReturn[time(wGMVL1), ]) / 100
CVARRetFac <- 1 + rowSums(wCVAR1 * StockReturn[time(wCVAR1), ]) / 100

# ベース値
GMVRetFac[1] <- 100
CVARRetFac[1] <- 100

# 累積リターン
# --- Min-Vol
# --- CVaR
GMVPort <- GMVRetFac %>% cumprod() %>% timeSeries(charvec = names(GMVRetFac))
CVARPort <- CVARRetFac %>% cumprod() %>% timeSeries(charvec = names(CVARRetFac))


# 5 プロット比較 -----------------------------------------------------------

# プロット作成
# --- 累積リターン
GMVPort %>%
  cbind(CVARPort) %>%
  set_colnames(c("Minimum-Variance", "Minimum-CVaR")) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Period") %>%
  pivot_longer(-Period, names_to = "Series", values_to = "Return") %>%
  ggplot(aes(x = Period, y = Return, group = Series, color = Series)) +
  geom_line() +
  ylab("Portfolio Value (Index)")

# プロット作成
# --- 累積超過リターン
GMVPort %>%
  cbind(CVARPort) %>%
  set_colnames(c("Minimum_Variance", "Minimum_CVaR")) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Period") %>%
  mutate(Excess = (Minimum_CVaR - Minimum_Variance) / Minimum_Variance * 100) %>%
  ggplot(aes(x = Period, y = Excess)) +
  geom_col() +
  ylab("Percent") +
  ggtitle("Relative Out-Performance Min-CVaR vs. Min-Variance")
