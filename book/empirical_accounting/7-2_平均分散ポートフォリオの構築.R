# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 7 ファクター・モデルの応用
# Theme   : 2 平均分散ポートフォリオの構築
# Date    : 2022/10/19
# Page    : P319 - P332
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - FF3をリスクモデルと見立ててポートフォリオ最適化を行う


# ＜目次＞
# 0 準備
# 1 ファクターの分散共分散行列の推定
# 2 対象企業の抽出
# 3 誤差項の分散共分散行列の推定
# 4 リターンの分散共分散行列を準備
# 5 期待リターンの作成
# 6 最適化によるポートフォリオ構築
# 7 最適ポートフォリオのシミュレーション
# 8 効率的フロンティア曲線のプロット


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(broom)
library(quadprog)


# データロード
monthly_data <- read_csv("data/ch05_output1.csv")
annual_data  <- read_csv("data/ch05_output2.csv")
factor_data <- read_csv("data/ch06_output.csv")
FF3_loadings <- read_csv("data/ch07_FF3_loadings.csv")
FF3_cost_of_capital <- read_csv("data/ch07_FF3_cost_of_capital.csv")


# 1 ファクターの分散共分散行列の推定 -----------------------------------------------

# 分散共分散行列の作成
# --- 月次ファクターリターンから計算
# --- 12を掛けて年次データに換算
Sigma_FF3 <-
  factor_data %>%
    select(R_Me, SMB, HML) %>%
    cov() %>%
    multiply_by(12)


# 2 対象企業の抽出 ---------------------------------------------------------------

# パラメータ設定
# --- 投資対象の企業を100社に限定
N_portfolio_firms <- 100

# 対象企業の抽出
investment_universe <-
  annual_data %>%
    filter(year == 2020) %>%
    filter(rank(desc(ME)) <= N_portfolio_firms) %>%
    select(firm_ID) %>%
    unlist() %>%
    set_names(NULL)


# 3 誤差項の分散共分散行列の推定 -----------------------------------------------------

# 対角成分の作成
# --- ファクターローディング(エクスポージャー)は全期間で同様の値を使用
# --- FF3による期待リターンと残差リターンを計算
# --- 残差リターンから共分散行行列を作成して年率化
Sigma_epsilon <-
  monthly_data %>%
    filter(firm_ID %in% investment_universe) %>%
    left_join(FF3_loadings, by = "firm_ID") %>%
    left_join(select(factor_data, -R_F), by = "month_ID") %>%
    mutate(R_FF3 = R_F + beta_M * R_Me + beta_SMB * SMB + beta_HML * HML,
           epsilon = R - R_FF3) %>%
    group_by(firm_ID) %>%
    summarize(epsilon_variance = 12 * var(epsilon, na.rm = TRUE)) %>%
    select(epsilon_variance) %>%
    unlist() %>%
    diag()

# 確認
Sigma_epsilon[1:6, 1:6] %>% print()


# 4 リターンの分散共分散行列の作成--------------------------------------------------

# ファクターローディングの抽出
beta <-
  FF3_loadings %>%
    filter(firm_ID %in% investment_universe) %>%
    select(-firm_ID) %>%
    as.matrix()

# 分解式に基づいて分散共分散行列を計算
Sigma <- beta %*% Sigma_FF3 %*% t(beta) + Sigma_epsilon

# 確認
Sigma[1:6, 1:6] %>% print()


# 5 期待リターンの作成 ------------------------------------------------------------

# リターンベクトルの作成
# --- 7-1の資本コスト推定の際に作成したFF3の期待リターンを使用
mu <-
  FF3_cost_of_capital %>%
    filter(firm_ID %in% investment_universe) %>%
    select(-firm_ID) %>%
    unlist() %>%
    set_names(NULL)


# 6 最適化によるポートフォリオ構築 -------------------------------------------------

# パラメータ設定
# --- 目標期待リターンを0.1に設定
target_return <- 0.1

# パーツ設定
# --- 分散共分散行列
# --- 対応項無し
# --- 目標期待リターン，及び保有比率の係数
# --- 目標期待リターン，及び保有比率の合計値
Dmat <- Sigma
dvec <- rep(0, N_portfolio_firms)
Amat <- cbind(mu, rep(1, N_portfolio_firms))
bvec <- c(target_return, 1)

# 等号制約の数をmeq引数で表す
MV_portfolio <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)

# 確認
MV_portfolio %>% glimpse()

# 最適ウエイトの取得
optimal_weight <- MV_portfolio$solution
optimal_weight %>% print()

# リスクの取得
minimized_risk <- sqrt(2 * MV_portfolio$value)
minimized_risk %>% print()


# 7 最適ポートフォリオのシミュレーション ------------------------------------------

# パラメータ設定
# --- シミュレーション回数
N_points <- 100

# 目標期待リターンの作成
# --- -0.1から0.4の範囲で等差数列により作成
target_return <- seq(-0.1, 0.4, length = N_points)

# 格納オブジェクトの定義
optimal_weight <- matrix(NA, nrow = N_points, ncol = N_portfolio_firms)
minimized_risk <- rep(NA, N_points)

# 平均分散ポートフォリオの計算を100回繰り返す
for (i in seq_along(target_return)) {
  bvec <- c(target_return[i], 1)
  MV_portfolio <- solve.QP(Dmat, dvec, Amat, bvec, meq = 2)
  optimal_weight[i, ] <- MV_portfolio$solution # 結果の保存
  minimized_risk[i] <- sqrt(2 * MV_portfolio$value)
}

# ウエイト確認
optimal_weight[N_points, 1:10] %>% print()

# リスク
minimized_risk[N_points] %>% print()


# 8 効率的フロンティア曲線のプロット -------------------------------------------

# データフレーム作成
# --- シミュレーションの期待リターンとリスク
MV_frontier <- tibble(target_return, minimized_risk)

# プロット作成
# --- 効率的フロンティア曲線
MV_frontier %>%
  ggplot(aes(x = target_return, y = minimized_risk)) +
    geom_line() +
    geom_point() +
    coord_flip() +
    labs(x = "Expected Return", y = "Risk") +
    theme_tq()
