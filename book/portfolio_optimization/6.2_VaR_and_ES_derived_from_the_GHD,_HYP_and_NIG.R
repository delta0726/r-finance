# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 2 VaR and ES deriverd from the GHD, HYP and NIG
# Date    : 2022/10/31
# Page    : P77 - P80
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 HP社のリターン系列を作成
# 2 分布モデルによるフィッティング
# 3 VaRの算出
# 4 VaRのプロット
# 5 ESの算出
# 6 ESのプロット


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(ghyp)
library(timeSeries)
library(fBasics)


# データロード
data(DowJones30)

# データ確認
DowJones30 %>% class()
DowJones30 %>% glimpse()


# 1 HP社のリターン系列を作成 ----------------------------------------------------

# データ加工
# --- Period列を作成
DowJones30_Mod <-
  DowJones30 %>%
    mutate(Period = as.character(X.Y..m..d)) %>%
    as_tibble()

# 日次リターンの作成
# --- HP社のみを抽出
yret <-
  DowJones30_Mod$HWP %>%
    timeSeries(charvec = DowJones30_Mod$Period) %>%
    log() %>%
    diff() %>%
    multiply_by(100) %>%
    na.omit()

# データ確認
yret %>% length()
yret %>% hist()


# 2 分布モデルによるフィッティング -------------------------------------------------

# 各分布でフィッティング
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
ghdfit <- yret %>% fit.ghypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
hypfit <- yret %>% fit.hypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
nigfit <- yret %>% fit.NIGuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)


# 3 VaRの算出 -----------------------------------------------------------------

# 確率点の作成
p <- seq(from = 0.001, to = 0.05, by = 0.001)

# VaRの算出
ghd.VaR <- qghyp(p, ghdfit) %>% abs()
hyp.VaR <- qghyp(p, hypfit) %>% abs()
nig.VaR <- qghyp(p, nigfit) %>% abs()
nor.VaR <- qnorm(p, mean = mean(yret), sd = sd(yret)) %>% abs()
emp.VaR <- quantile(x = yret, probs = p) %>% abs()


# 4 VaRのプロット ------------------------------------------------------------

# データフレーム変換
df_var <-
  tibble(type = "Empirical", p = p, var = emp.VaR) %>%
    bind_rows(tibble(type = "GHD", p = p, var = ghd.VaR)) %>%
    bind_rows(tibble(type = "HYP", p = p, var = hyp.VaR)) %>%
    bind_rows(tibble(type = "NIG", p = p, var = nig.VaR)) %>%
    bind_rows(tibble(type = "Normal", p = p, var = nor.VaR))

# プロット作成
df_var %>%
  ggplot(aes(x = p, y = var, color = type, fill = type, group = type)) +
  geom_line() +
  xlab("Percent point of Distribution") +
  ylab("Value at Risk (%)") +
  ggtitle("Compare with VaR by several Distributions")


# 5 ESの算出 -----------------------------------------------------------------

## ES
ghd.ES <- ESghyp(p, ghdfit) %>% abs()
hyp.ES <- ESghyp(p, hypfit) %>% abs()
nig.ES <- ESghyp(p, nigfit) %>% abs()
nor.ES <- abs(mean(yret) - sd(yret) * dnorm(qnorm(1 - p)) / p)
obs.p <- ceiling(p * length(yret))
emp.ES <- sapply(obs.p, function(x) abs(mean(sort(c(yret))[1:x])))


# 6 ESのプロット ---------------------------------------------------------------

# データフレーム変換
df_es <-
  tibble(type = "Empirical", p = p, es = emp.ES) %>%
    bind_rows(tibble(type = "GHD", p = p, es = ghd.ES)) %>%
    bind_rows(tibble(type = "HYP", p = p, es = hyp.ES)) %>%
    bind_rows(tibble(type = "NIG", p = p, es = nig.ES)) %>%
    bind_rows(tibble(type = "Normal", p = p, es = nor.ES))

# プロット作成
df_es %>%
  ggplot(aes(x = p, y = es, color = type, fill = type, group = type)) +
  geom_line() +
  xlab("Percent point of Distribution") +
  ylab("Expected Short Fall (%)") +
  ggtitle("Compare with ES by several Distributions")
