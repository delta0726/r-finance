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
library(tidyquant)
library(magrittr)


# データロード
stock_data_raw <- read_csv("data/ch05_stock_data.csv")


# 1 データ加工 ----------------------------------------------------------------

# 加工済データの作成
# --- 以降で必要なデータのみ抽出
stock_data <-
  stock_data_raw %>%
    group_by(firm_ID) %>%
    mutate(lagged_stock_price = lag(stock_price)) %>%
    ungroup() %>%
    mutate(ME = stock_price * shares_outstanding,
           stock_price_adj = (stock_price + DPS) * adjustment_coefficient,
           R = (stock_price_adj - lagged_stock_price) / lagged_stock_price,
           Re = R - R_F) %>%
    select(year, month, month_ID, firm_ID, R, R_F, Re)


# 2 リターンデータに対する仮定 ---------------------------------------------------

# ＜ポイント＞
# - 月次リターンは独立であり系列相関は持たない
#   --- ボラティリティは系列相関を持つが、リターンのディレクションに系列相関はない


# 時系列プロット作成
# --- firm_IDが1の企業のデータのみ抽出
# --- x軸にmonth_ID, y軸に月次超過リターンを表示
stock_data %>%
  filter(firm_ID == 1) %>%
  ggplot(aes(x = month_ID, y = Re)) +
  geom_bar(stat = "identity") +
  labs(x = "Month ID", y = "Firm 1's Monthly Excess Return") +
  scale_x_continuous(expand = c(0.01, 0)) +
  theme_tq()

# ヒストグラム作成
stock_data %>%
  filter(firm_ID == 1) %>%
  ggplot(aes(x = Re)) +
  geom_histogram() +
  labs(x = "Firm 1's Monthly Excess Return", y = "Count") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_tq()


# 3 推定量と推定値の違い --------------------------------------------------------

# ＜ポイント＞
# - 観測されたデータの平均値は｢推定量｣又は｢標本平均｣と呼ばれる
#   --- 仮に無限個のデータを観測できたら｢標本平均｣は｢真の期待値｣に収束する（大数の法則）
#   --- 現実には有限個のサンプルしか観測できないため、ランダムサンプリングによる誤差の影響を受ける

# ＜推定量と推定値の違い＞
# - 推定量；特定の観測データに基づく実現値（1つのリターンサンプルから計算した平均値）
# - 推定量；観測データの関数として表される確率変数（様々なリターンサンプルから計算した平均値の確率分布）


# 平均値の推定値
# --- 推定量
stock_data %>%
  filter(firm_ID == 1) %>%
  use_series(Re) %>%
  mean(na.rm = TRUE)


# 4 月次超過リターンのt値の計算 ---------------------------------------------------

# ＜ポイント＞
# - 1つの平均値から有意に｢r>0｣であることを示すことはできない
#   --- t値を使うとサンプルの標準誤差を考慮するので論理性が高まる


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
# --- t値（推定値が期待値からどの程度離れているかの統計量）
t_value <- (mean(Re_firm_ID_1) - mu0) / sqrt(var(Re_firm_ID_1) / n)


# 5 統計的検定の考え方 -------------------------------------------------------------

# t検定の実施
Re_firm_ID_1 %>% t.test()
