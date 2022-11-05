# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 9 Modeling Dependence
# Theme   : 5.2 Mixed copula approaches
# Date    : 2022/11/05
# Page    : P155 - P157
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 コピュラの定義
# 3 最適化


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(copula)
library(FRAPO)
library(QRM)

# データロード
data(DJ.df)

# データ確認
DJ.df %>% as_tibble()


# 1 データ準備 -------------------------------------------------------------------

# データ作成
# --- 経験分布関数
U <-
  DJ.df[, c("GM", "UTX")] %>%
    returnseries(method = "discrete", trim = TRUE) %>%
    apply( 2, QRM::edf)


# 2 コピュラの定義 -------------------------------------------------------------------

# オブジェクト初期化
# --- 重み付き密度からなる対数尤度を作成
# --- クレイトン コピュラ
# --- ガンベル コピュラ
copC <- claytonCopula(2)
copG <- gumbelCopula(2)

# 関数定義
LLCG <- function(params, x, copC, copG){
  slot(copC, "parameters") <- params[1]
  slot(copG, "parameters") <- params[2]
  pi <- params[3]
  ldens <- log(pi * dCopula(x, copC) + (1 - pi) * dCopula(x, copG))
  if(any(is.infinite(ldens))){
      ldens[which(is.infinite(ldens))] <- 0
  }
  sum(ldens)
}

# パラメータ設定
lower <- c(copC@param.lowbnd, copG@param.lowbnd, 0.0)
upper <- c(copC@param.upbnd, copG@param.upbnd, 1.0)

# パラメータ設定
par1 <- copC %>% copula::fitCopula(U, "itau") %>% .@estimate
par2 <- copG %>% copula::fitCopula(U, "itau") %>% .@estimate
par3 <- 0.5


# 3 最適化 -------------------------------------------------------------------

## Optimisation
opt <- optim(c(par1, par2, par3), LLCG, x = U,
             copC = copC,
             copG = copG,
             lower = lower,
             upper = upper,
             method = "L-BFGS-B",
             control = list(fnscale = -1, trace = 2),
             hessian = TRUE)

## Variance-Covariance
varcov <- -opt$hessian %>% solve() %>% round(4)

# 結果確認
varcov