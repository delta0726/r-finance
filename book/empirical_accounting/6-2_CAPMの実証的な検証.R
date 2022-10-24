# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 6 ファクターモデルの準備
# Theme   : 2 CAPMの実証的な検証
# Date    : 2022/10/25
# Page    : P256 - P271
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - CAPMとは市場においてベータという軸が存在することを示すもの
#   --- CAPMは非現実的な仮定に基づいく理論であるが、市場の本質的なエッセンスを示すもの
#   --- 現実のマーケットでCAPMを実行することで、市場の歪みがどこに存在するのか調べるヒントとなる


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 1分位のみで時系列回帰
# 3 ポートフォリオごとの回帰
# 4 CAPMアルファの可視化
# 5 CAPMアルファの統計的な有意性を評価
# 6 証券市場線の推定


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(broom)


# データロード
factor_data <- read_csv("data/ch06_factor_data.csv")
ME_sorted_portfolio <- read_csv("data/ch06_ME_sorted_portfolio.csv")
ME_cross_sectional_return <- read_csv("data/ch06_ME_cross_sectional_return.csv")

# データ確認
# --- 市場ポートフォリオのリターン（月次データ）
# --- 分位ポートフォリオのリターン（月次/分位別データ）
factor_data %>% print()
ME_sorted_portfolio %>% print()


# 1 データ準備 ------------------------------------------------------------------

# ＜ポイント＞
# 分位ポートフォリオに市場リターンを追加する


# データ結合
# --- 分位ポートフォリオに市場リターンを追加
ME_sorted_portfolio <-
  ME_sorted_portfolio %>%
    full_join(select(factor_data, -R_F), by = "month_ID") %>%
    select(month_ID, ME_rank10 ,Re , R_M, R_Me)

# データ確認
ME_sorted_portfolio %>% print()

# データ保存
# ME_sorted_portfolio %>% write_csv("data/ch06_ME_sorted_portfolio_2.csv")


# 2 1分位のみで時系列回帰 ----------------------------------------------------------

# ＜ポイント＞
# - CAPMは証券iのリスクプレミアムを市場リスクプレミアムで回帰する
#   --- 証券iを分位ポートフォリオに変更して分析を行う（誤差項の影響を小さくする）
#   --- 単一銘柄に対して時系列方向に回帰するので時系列回帰と呼ぶ

# ＜CAPMの世界＞
# - 個別銘柄のリスクプレミアムはベータのみで説明することができる
#   --- アルファは存在しないことを確認するため、回帰モデルに定数項を追加
#   --- 定数項がゼロ付近で統計的有意であればCAPMが成立


# データ抽出
# --- 分位ポートフォリオの抽出(F1)
# --- 1銘柄の時系列データ
ME_sorted_portfolio_F1 <-
  ME_sorted_portfolio %>%
    filter(ME_rank10 == 1)

# モデル構築
# --- CAPMの実行（証券iのリスクプレミアムを市場リスクプレミアムで回帰）
# --- 時系列方向のデータセットであるため時系列回帰
# --- Re(Total Return) ~ R_Me(Excess Return)
model_F1 <- lm(Re ~ R_Me, data = ME_sorted_portfolio_F1)

# 回帰係数の確認
# --- ベータは0.654と正の値となっている
# --- アルファは0.0121かつt値が3なので有意に正（CAPMが成立しない可能性が高い）
# --- CAPMはベータのみでリターンを説明するが、以下の結果はアルファが有意に惣菜することを示す
model_F1 %>% tidy()

# プロット作成
# --- 上記のモデルは使用せず、geom_smooth()で回帰直線を描いている
# --- 切片が有意に正となっている（回帰直線の信頼区間から判別）
ME_sorted_portfolio_F1 %>%
  ggplot(aes(x = R_Me, y = Re)) +
  geom_point() +
  geom_smooth(method = "lm", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Excess Return of Market Portfolio", y = "Excess Return of Small Size Portfolio") +
  theme_tq()


# 3 ポートフォリオごとの回帰 ------------------------------------------------------

# ＜ポイント＞
# - 全ての分位で時系列回帰を行ってCAPMモデルの回帰係数を取得する
#   --- purrr::map()を使ったモダンな方法のみを示す


# CAPMの実行
# --- 各モデルの回帰係数の抽出
CAPM_results <-
  ME_sorted_portfolio %>%
    group_by(ME_rank10) %>%
    nest() %>%
    mutate(CAPM_regression = map(data, ~ lm(Re ~ R_Me, data = .)),
           CAPM_summary = map(CAPM_regression, tidy)) %>%
    select(-c(data, CAPM_regression)) %>%
    unnest(cols = CAPM_summary) %>%
    ungroup()

# 確認
CAPM_results %>% print()


# 4 CAPMアルファの可視化 ---------------------------------------------------------

# ＜ポイント＞
# - CAPMアルファ(切片項)を分位ごとに棒グラフで表示
#   --- 0付近であるかどうかを確認
#   --- 標準誤差がわからないので回帰係数だけでは有意性は判断できない


# プロット作成
# --- 定数項(CAPMアルファ)を抽出してプロット
CAPM_results %>%
  filter(term == "(Intercept)") %>%
  mutate(ME_rank10 = as.factor(ME_rank10)) %>%
  ggplot() +
  geom_col(aes(x = ME_rank10, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "ME Rank", y = "CAPM alpha") +
  scale_y_continuous(limits = c(-0.003, 0.013)) +
  theme_tq()


# 5 CAPMアルファの統計的な有意性を評価 ----------------------------------------------

# ＜ポイント＞
# - CAPMアルファの有意性をp値で判断する
#   --- 時価総額が小さい分位ほどCAPMが成立していない(CAPMアルファが存在する)
#   --- サイズ効果がCAPMアルファとして現れている


# CAPMアルファの有意性評価
# --- p値に応じてスターを付ける
CAPM_results %>%
  filter(term == "(Intercept)") %>%
  rename(CAPM_alpha = estimate, p_value = p.value) %>%
  mutate(significance = cut(p_value,
                            breaks = c(0, 0.01, 0.05, 0.1, 1),
                            labels = c("***", "**", "*", ""),
                            include.lowest = TRUE)) %>%
  select(ME_rank10, CAPM_alpha, p_value, significance)


# 6 証券市場線の推定 --------------------------------------------------------------

# ＜ポイント＞
# - 証券市場線(SML)とは市場ポートフォリオ(β=1)の期待リターンと原点を結んだ線を指す
# - CAPMアルファとは散布図の各点とSMLとの乖離幅を指す
#   --- 各点がSMLから乖離していることがCAPMが成立していない実証的な証拠


# データ作成
# --- CAPMベータとリスクプレミアムを取得
ME_cross_sectional_return <-
  CAPM_results %>%
    filter(term == "R_Me") %>%
    rename(CAPM_beta = estimate) %>% #
    select(ME_rank10, CAPM_beta) %>%
    full_join(ME_cross_sectional_return, ., by = "ME_rank10") %>%
    mutate(ME_rank10 = as.factor(ME_rank10))

# 証券市場線の傾き
# --- 市場ポートフォリオの実現超過リターンの平均値
# --- 証券市場線は原点からの市場ポートフォリオへの傾き
mean_R_Me <- factor_data$R_Me %>% mean()

# プロット作成
# --- CAPMベータと期待リターンの散布図
# --- 証券市場線の追加
ME_cross_sectional_return %>%
  ggplot(aes(x = CAPM_beta, y = mean_Re)) +
  geom_point() +
  geom_abline(intercept = 0, slope = mean_R_Me) +
  labs(x = "Market beta", y = "Mean Excess Return") +
  scale_x_continuous(limits = c(0, 1.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 0.02)) +
  theme_tq()
