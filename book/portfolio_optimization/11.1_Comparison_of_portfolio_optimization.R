# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 11 Diversification roconsidered
# Theme   : 1 Comparison of Portfolio Optimization
# Date    : 2022/11/03
# Page    : P212 - P216
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 分散共分散行列の計算
# 2 ポートフォリオ最適化
# 3 リスク寄与度の計算
# 4 アロケーション比較
# 5 銘柄ごとのアロケーション比較
# 6 リスク指標の確認


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(FRAPO)
library(fPortfolio)
library(PerformanceAnalytics)


# データロード
data(SPISECTOR)

# データ確認
SPISECTOR %>% head()
SPISECTOR[, -"SPI"] %>% head()


# 1 分散共分散行列の計算 ---------------------------------------------------------

# リターン
R <-
  SPISECTOR %>%
    interpNA(method = "before") %>%
    returnseries(method = "discrete", trim = TRUE)

# 分散共分散行列
V <- R %>% cov()


# 2 ポートフォリオ最適化 ---------------------------------------------------------

# 最適ウエイト
# --- Global Minimum Variance Portfolio
# --- Most Diversified Portfolio
# --- Minimum Tail Dependent Portfolio(CVaR最小化)
# --- Equal risk contributed portfolios(Risk Parity)
GMVw <- R %>% PGMV() %>% Weights()
MDPw <- R %>% PMD() %>% Weights()
MTDw <- R %>% PMTD() %>% Weights()
ERCw <- V %>% PERC() %>% Weights()

# ウエイト結合
W <- cbind(GMVw, MDPw, MTDw, ERCw)

# プロット比較
W %>%
  as.data.frame() %>%
  rownames_to_column(var = "TICKER") %>%
  pivot_longer(-TICKER, names_to = "Type", values_to = "Weight") %>%
  ggplot(aes(x = TICKER, y = Weight, color = Type, fill = Type)) +
  geom_col() +
  facet_wrap(~Type)


# 3 リスク寄与度の計算 ------------------------------------------------------------

# リスク寄与度
MRC <-
  W %>%
    apply(MARGIN =  2, FUN = mrc, Sigma = V) %>%
    set_colnames(c("GMV", "MDP", "MTD", "ERC")) %>%
    set_rownames(names(SPISECTOR))

# 確認
MRC %>% apply(2, sum)

# プロット比較
MRC %>%
  as.data.frame() %>%
  rownames_to_column(var = "TICKER") %>%
  pivot_longer(-TICKER, names_to = "Type", values_to = "MCR") %>%
  ggplot(aes(x = TICKER, y = MCR, color = Type, fill = Type)) +
  geom_col() +
  facet_wrap(~Type)


# 4 アロケーション比較 -----------------------------------------------------------

# プロット設定
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))

# 最小分散
dotchart(GMVw, xlim = c(0, 40), main = "GMV Allocation", pch = 19)

# 最大分散 - 最小分散
dotchart(MDPw - GMVw, xlim = c(-20, 20), main = "MDP vs. GMV", pch = 19)
abline(v = 0, col = "grey")

# CVaR最小化 - 最小分散
dotchart(MTDw - GMVw, xlim = c(-20, 20), main = "MTD vs. GMV", pch = 19)
abline(v = 0, col = "grey")

# リスクパリティ - 最小分散
dotchart(ERCw - GMVw, xlim = c(-20, 20), main = "ERC vs. GMV", pch = 19)
abline(v = 0, col = "grey")
par(oldpar)


# 5 銘柄ごとのアロケーション比較 -----------------------------------------------

# ウエイト
W %>%
  as.data.frame() %>%
  rownames_to_column(var = "TICKER") %>%
  pivot_longer(-TICKER, names_to = "Type", values_to = "Weight") %>%
  ggplot(aes(x = Type, y = Weight, color = TICKER, fill = TICKER)) +
  geom_col() +
  facet_wrap(~TICKER) +
  coord_flip() +
  theme(legend.position = "none")

# リスク寄与度
MRC %>%
  as.data.frame() %>%
  rownames_to_column(var = "TICKER") %>%
  pivot_longer(-TICKER, names_to = "Type", values_to = "MCR") %>%
  ggplot(aes(x = Type, y = MCR, color = TICKER, fill = TICKER)) +
  geom_col() +
  facet_wrap(~TICKER) +
  coord_flip() +
  theme(legend.position = "none")


# 6 リスク指標の確認 ------------------------------------------------------------

# ポートフォリオリターン
Rdec <- R / 100
Pret <- W %>% apply( 2, function(x) Rdec %*% x / 100)

# ボラティリティ
SD_raw <- Pret %>% apply(MARGIN = 2, sd) * 100
SD_ann <- Pret %>% apply(MARGIN = 2, function(x) sd(x) * sqrt(260)) * 100

# 期待ショートフォール
ES95 <- Pret %>% apply(MARGIN = 2, function(x) abs(ES(R = x, method = "modified") * 100))

# 分散レシオ
# Diversification Ratio
# concentration Ratio
DR <- W %>% apply(MARGIN = 2, FUN = dr, Sigma = V)
CR <- W %>% apply(MARGIN = 2, FUN = cr, Sigma = V)

# 確認
rbind(SD_raw, SD_ann, ES95, DR, CR)
