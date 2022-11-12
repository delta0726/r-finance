# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 6 Suitable Distribution for returns
# Theme   : 6.2 Risk assessment with the GHD
# Date    : 2022/11/11
# Page    : P77 - P80
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# -  VaRとESを経験的分布と各種分布で算出して比較する
#    --- テールリスクの推定が経験的分布と近くなるのが一般化双曲型分布(GHD)であることを確認する
#    --- リターン系列が短い際は経験的分布のVaRなどの信頼性が下がることへのソリューション


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
library(conflicted)

# コンフリクト解消
conflict_prefer("select", "dplyr")


# データロード
data(DowJones30)

# データ確認
DowJones30 %>% class()
DowJones30 %>% as_tibble()


# 1 HP社のリターン系列を作成 ----------------------------------------------------

# ＜ポイント＞
# - HP社のリターン系列から経験的分布を作成する
#   --- 正規分布よりも過剰尖度となっている（統計量より判断）


# データ加工
# --- Period列を作成
DowJones30_Mod <-
  DowJones30 %>%
    mutate(Period = as.character(X.Y..m..d)) %>%
    as_tibble()

# 日次リターンの作成
# --- HP社のみを抽出
yret <-
  DowJones30_Mod %>%
    select(Period, HWP) %$%
    timeSeries(data = HWP, charvec = Period) %>%
    log() %>%
    diff() %>%
    multiply_by(100) %>%
    na.omit()

# データ確認
yret %>% head()
yret %>% length()

# 尖度の確認
# --- 3を超えている（正規分布より過剰尖度）
yret %>% kurtosis()

# プロット確認
yret %>%
  as.data.frame() %>%
  ggplot(aes(x = TS.1)) +
  geom_density()


# 2 分布モデルによるフィッティング -------------------------------------------------

# ＜ポイント＞
# - 分布ごとにモデルを構築する


# 各分布でフィッティング
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
ghdfit <- yret %>% fit.ghypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
hypfit <- yret %>% fit.hypuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)
nigfit <- yret %>% fit.NIGuv(symmetric = FALSE, control = list(maxit = 1000), silent = TRUE)


# 3 VaRの算出 -----------------------------------------------------------------

# ＜ポイント＞
# - VaRは分布における分位点における裾の面積として定義される（分位点以上の発生確率）
#   --- 2で作成した分布モデルはqghyp()により定義される
#   --- 正規分布はqnorm()により定義される
#   --- 経験的分布は単純な分位点のためquantile()により定義される


# 分位点(確率点)の作成
p <- seq(from = 0.001, to = 0.05, by = 0.001)

# VaRの算出
#   --- GHD：一般化双曲型分布
#   --- HYP：双曲型分布
#   --- NIG：正規逆ガウス分布
#   --- 正規分布
#   --- 経験的分布（単純な分位点）
ghd.VaR <- ghdfit %>% qghyp(p, object = .) %>% abs()
hyp.VaR <- hypfit %>% qghyp(p, object = .) %>% abs()
nig.VaR <- nigfit %>% qghyp(p, object = .) %>% abs()
nor.VaR <- qnorm(p, mean = mean(yret), sd = sd(yret)) %>% abs()
emp.VaR <- yret %>% quantile(probs = p) %>% abs()


# 4 VaRのプロット ------------------------------------------------------------

# ＜ポイント＞
# - 経験的分布と最も近いのは一般化双曲型分布(GHD)となっている
#   --- 0付近では乖離が大きくなるが、それ以外は経験的分布とほぼ一致している
# - 正規分布は分位点が小さくなるほど(左)VaRの推定が経験的分布と乖離していく
#   --- 正規分布から導き出されたVaRは保守的すぎるため、投資家はリスクを取れないことに不満を覚える可能性がある


# データフレーム変換
df_var <-
  tibble(type = "Empirical", p = p, var = emp.VaR) %>%
    bind_rows(tibble(type = "GHD", p = p, var = ghd.VaR)) %>%
    bind_rows(tibble(type = "HYP", p = p, var = hyp.VaR)) %>%
    bind_rows(tibble(type = "NIG", p = p, var = nig.VaR)) %>%
    bind_rows(tibble(type = "Normal", p = p, var = nor.VaR))

# プロット作成
# --- 分位点ごとのVaRの大きさをプロット
# --- 分布の裾のプロットではない点に注意
df_var %>%
  ggplot(aes(x = p, y = var, color = type, fill = type, group = type)) +
  geom_line() +
  xlab("Percent point of Distribution") +
  ylab("Value at Risk (%)") +
  ggtitle("Compare with VaR by several Distributions")


# 5 ESの算出 -----------------------------------------------------------------

# 分位点(確率点)の作成
p <- seq(from = 0.001, to = 0.05, by = 0.001)

# 関数定義
nor_es <- function(yret ,p){
  mean(yret) - sd(yret) * dnorm(qnorm(1 - p)) / p
}

# 関数定義
# --- 経験的分布の期待ショートフォール(ES)
emp_es <- function(yret ,p){
  p_order <- ceiling(p * length(yret))
  yret_sort <- yret %>% as.vector() %>% sort()
  p_order %>% sapply(function(x) yret_sort[1:x] %>% mean())
}

# ES
ghd.ES <- ghdfit %>% ESghyp(p, .) %>% abs()
hyp.ES <- hypfit %>% ESghyp(p, .) %>% abs()
nig.ES <- nigfit %>% ESghyp(p, .) %>% abs()
nor.ES <- yret %>% nor_es(p) %>% abs()
emp.ES <- yret %>% emp_es(p) %>% abs()


# 6 ESのプロット ---------------------------------------------------------------

# ＜ポイント＞
# - 正規分布は分位点が小さくなるほど(左)ESの推定が経験的分布と乖離していく
# - 経験的分布と最も近いのは一般化双曲型分布(GHD)となっている
#   --- ESが計算される際に過小推定が累積されるため、乖離が全般的に大きくなる


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
