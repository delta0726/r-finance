# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 5 株式データの取得と可視化
# Theme   : 6 線形回帰入門
# Date    : 2022/10/09
# Page    : P225 - P236
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - 株式財務データを用いて線形回帰モデルを復習する


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 散布図と回帰直線
# 3 最小二乗法によるモデル構築
# 4 {broom}によるモデル解釈
# 5 対数回帰モデル


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)


# データロード
annual_data <- read_csv("data/ch05_output2.csv")


# 1 データ加工 ----------------------------------------------------------------

# モデルデータの作成
lm_sample_data <-
  annual_data %>%
    group_by(firm_ID) %>%
    mutate(lagged_BEME = lagged_BE / lag(ME)) %>%
    ungroup() %>%
    filter(year == 2016, firm_ID <= 10) %>%
    select(firm_ID, year, Re, lagged_BEME) %>%
    drop_na()


# 2 散布図と回帰直線 ----------------------------------------------------------

# プロット作成
# --- x軸に簿価時価比率, y軸に超過リターン
p <-
  lm_sample_data %>%
    ggplot(aes(x = lagged_BEME, y = Re)) +
      geom_point() +
      labs(x = "BE/ME at the End of Year t", y = "Excess Return for Year t + 1") +
      theme_classic()

# 確認
print(p)

# 回帰直線の追加
# --- geom_smooth()関数を用いる
p +  geom_smooth(aes(x = lagged_BEME, y = Re), method = "lm", se = FALSE, color = "black")


# 3 最小二乗法によるモデル構築 ------------------------------------------------

# モデル構築
# --- "~"の左に従属変数, 右に独立変数を記す
lm_results <- lm(Re ~ lagged_BEME, data = lm_sample_data)

# 確認
lm_results %>% print()
lm_results %>% names()

# 回帰係数
lm_results$coefficients


# 4 {broom}によるモデル解釈 ------------------------------------------------------

# 回帰係数
# --- モデルの係数
lm_results %>% tidy()

# モデル統計量
# --- モデル全体の当てはまり度合い
lm_results %>% glance()


# 5 対数回帰モデル -----------------------------------------------------------

# モデル構築
lm_log_results <- lm(Re ~ log(lagged_BEME), data = lm_sample_data)

# 回帰係数
lm_log_results %>% tidy()

# モデル統計量
lm_log_results %>% glance()
