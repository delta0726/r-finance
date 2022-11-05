# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 9 Modeling Dependence
# Theme   : 5.1 GARCH-copula model: expected shortfall
# Date    : 2022/11/05
# Page    : P148 - P154
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 GARCHモデルの構築
# 3 コピュラの計算
# 4 期待ショートフォールの計算


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(QRM)
library(fGarch)

# データロード
data(EuStockMarkets)

# データ確認
EuStockMarkets %>% class()
EuStockMarkets %>% head()


# 1 データ準備 ------------------------------------------------------------------

# リターン計算
# --- 正負を逆転
loss <-
  EuStockMarkets %>%
    log() %>%
    diff() %>%
    multiply_by(-100) %>%
    na.omit() %>%
    as.data.frame()


# 2 GARCHモデルの構築 -------------------------------------------------------------

## GARCH
gfit <-
  loss %>%
    lapply(garchFit,  formula = ~ garch(1,1),
           cond.dist = "std", trace = FALSE)


gprog <-
  gfit %>%
    lapply(function(x) predict(x, n.ahead = 1)[3]) %>%
    unlist()


gshape <-
  gfit %>%
    lapply(function(x) x@fit$coef[5]) %>%
    unlist()

gresid <-
  gfit %>%
    lapply(function(x) x@residuals / sqrt(x@h.t)) %>%
    data.frame() %>%
    as.matrix()


# 3 コピュラの計算 ----------------------------------------------------------------

## Copula
U <- sapply(1:4, function(y) pt(gresid[, y], df = gshape[y]))
cop <- fit.tcopula(Udata = U, method = "Kendall")
rcop <- rcopula.t(100000, df = cop$nu, Sigma = cop$P)
qcop <- sapply(1:4, function(x) qstd(rcop[, x], nu = gshape[x]))
ht.mat <- matrix(gprog, nrow = 100000, ncol = ncol(loss),
                 byrow = TRUE)
pf <- qcop * ht.mat


# 4 期待ショートフォールの計算 -------------------------------------------------------

## ES 95 percent
weights <- c(0.4, 0.2, 0.2, 0.2)
pfall <- (qcop * ht.mat) %*% weights
pfall.es95 <- pfall %>% sort() %>% tail(, 5000) %>% median()
pfall.es95
