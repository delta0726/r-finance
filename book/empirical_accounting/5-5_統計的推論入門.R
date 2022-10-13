# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 5 株式データの取得と可視化
# Theme   : 5 統計的推論入門
# Date    : 2022/10/09
# Page    : P215 - P224
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - 株式リターンのデータを用いてt検定について復習する


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 リターンデータに対する仮定
# 3 推定量と推定値の違い
# 4 月次超過リターンのt値の計算
# 5 統計的検定の考え方


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)


# データロード
stock_data <- read_csv("data/ch05_stock_data.csv")


# 1 データ加工 ----------------------------------------------------------------

# 加工済データの作成
stock_data <-
  stock_data %>%
    group_by(firm_ID) %>%
    mutate(lagged_stock_price = lag(stock_price)) %>%
    ungroup() %>%
    mutate(ME = stock_price * shares_outstanding,
           stock_price_adj = (stock_price + DPS) * adjustment_coefficient,
           R = (stock_price_adj - lagged_stock_price) / lagged_stock_price,
           Re = R - R_F) %>%
    select(year, month, month_ID, firm_ID, stock_price, stock_price_adj,
           lagged_stock_price, adjustment_coefficient, DPS, R, R_F, Re, ME)


# 2 リターンデータに対する仮定 ---------------------------------------------------

# 時系列プロット作成
# --- firm_IDが1の企業のデータのみ抽出
# --- x軸にmonth_ID, y軸に月次超過リターンを表示
stock_data %>%
  filter(firm_ID == 1) %>%
  ggplot(aes(x = month_ID, y = Re)) +
  geom_bar(stat = "identity") +
  labs(x = "Month ID", y = "Firm 1's Monthly Excess Return") +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_classic()

# ヒストグラム作成
stock_data %>%
  filter(firm_ID == 1) %>%
  ggplot(aes(x = Re)) +
  geom_histogram() +
  labs(x = "Firm 1's Monthly Excess Return", y = "Count") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


# 3 推定量と推定値の違い --------------------------------------------------------

# 平均値の推定値
# --- 母集団の期待値は観察不可能なので、観察されたデータの平均値は推定値に過ぎない
# --- 大数の法則によると、仮に無限個のデータが観測できた場合、標本平均は真の期待値に収束する
stock_data %>%
  filter(firm_ID == 1) %>%
  use_series(Re) %>%
  mean(na.rm = TRUE)


# 4 月次超過リターンのt値の計算 ---------------------------------------------------

# 超過リターン系列の取得
Re_firm_ID_1 <-
  stock_data %>%
    filter(firm_ID == 1) %>%
    drop_na(Re) %>%
    use_series(Re)

# パラメータ設定
# --- 帰無仮説を期待値0と設定
# --- 標本サイズ
mu0 <- 0
n <- Re_firm_ID_1 %>% length()

# 検定統計量の計算
# --- t値
t_value <- (mean(Re_firm_ID_1) - mu0) / sqrt(var(Re_firm_ID_1) / n)


# 5 統計的検定の考え方 -------------------------------------------------------------

# t検定の実施
Re_firm_ID_1 %>% t.test()
