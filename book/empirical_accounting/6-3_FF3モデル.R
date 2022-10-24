# ***********************************************************************************************
# Title   : 実証会計・ファイナンス（Rによる財務・株式データの取得）
# Chapter : 6 ファクターモデルの準備
# Theme   : 3 Fama-French 3ファクターモデル
# Date    : 2022/10/25
# Page    : P271 - P289
# URL     : https://www2.econ.osaka-u.ac.jp/~eaafinr/sect-3.html
# ***********************************************************************************************


# ＜概要＞
# - FF3モデルはCAPMにサイズとバリュエーションのファクターを追加したモデル
# - CAPMではサイズに比例してCAPMアルファが観測されたが、FF3ではどのようにアルファが出力されるか？
#   --- FF3アルファのサイズ相関はなくなり、統計的有意なアルファも減少している


# ＜目次＞
# 0 準備
# 1 データフレームの整備
# 2 Size/Bpによる分位ポートの作成
# 3 年次ポートフォリオを月次データに統合
# 4 分位ポートフォリオの平均リターン
# 5 分位ポートフォリオのリターン推移
# 6 スプレッドリターン(SMBとHML)の作成
# 7 FF3のモデル構築
# 8 FF3アルファの有意性評価


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(broom)


# データロード
annual_data <- read_csv("data/ch06_annual_data.csv")
monthly_data <- read_csv("data/ch06_monthly_data.csv")
factor_data <- read_csv("data/ch06_factor_data.csv")
ME_sorted_portfolio <- read_csv("data/ch06_ME_sorted_portfolio_2.csv")


# 1 データフレームの整備 --------------------------------------------------------

# ＜ポイント＞
# - 各データフレームは不要な列が多いので必要な列のみを抽出する
# - 列名も直観性のあるものに変更する


# annual_data
annual_data <-
  annual_data %>%
    rename(Rf = R_F,
           Mcap = lagged_ME,
           Bp = lagged_BEME) %>%
    select(year, firm_ID,  R, Re, Rf, Mcap, Bp)

# monthly_data
monthly_data <-
  monthly_data %>%
    rename(Rf = R_F) %>%
    select(year, firm_ID, month_ID, R, Rf)

# factor_data
factor_data <-
  factor_data %>%
    select(month_ID, R_Me)

# ME_sorted_portfolio
ME_sorted_portfolio <-
  ME_sorted_portfolio %>%
    select(month_ID, ME_rank10, Re)


# 2 Size/Bpによる分位ポートの作成 ------------------------------------------------------

# ＜ポイント＞
# - FF3で使用するSizeとBpの分位ポートフォリオを作成する
#   --- Sizeは2分位に変換（1：小型株  2：大型株）
#   --- Bpは3分位変換（1：グロース株  2：ニュートラル株 3：バリュー株）


# 年次データの加工
# --- 分位ポートフォリオの作成
# --- Bpはパーセントランクの閾値で分位化
# --- interaction()は文字列を結合してファクター化する関数
annual_tiled_data <-
  annual_data %>%
    group_by(year) %>%
    mutate(Mcap_tile2 = as.factor(ntile(Mcap, 2)),
           Bp_prank = percent_rank(Bp)) %>%
    ungroup() %>%
    mutate(Bp_rank3 = cut(Bp_prank,
                          breaks = c(0, 0.3, 0.7, 1),
                          labels = c(1, 2, 3),
                          include.lowest = TRUE)) %>%
    mutate(FF_port_type = interaction(Mcap_tile2, Bp_rank3)) %>%
    mutate(FF_port_type = fct_recode(FF_port_type,
                                     SL = "1.1", BL = "2.1",
                                     SN = "1.2", BN = "2.2",
                                     SH = "1.3", BH = "2.3")) %>%
    group_by(year, FF_port_type) %>%
    mutate(w = Mcap  / sum(Mcap, na.rm = TRUE)) %>%
    ungroup()

# データ確認
# --- P275
annual_tiled_data %>%
  select(firm_ID, year, R, Re, Rf, Mcap_tile2, Bp_prank, Bp_rank3, FF_port_type) %>%
  slice(1:6)

# 分位ポート確認
# --- Mcap_tile2とBp_rank3のペアでグループ化
annual_tiled_data %>%
  group_by(FF_port_type) %>%
  summarize(mean_Bp = mean(Bp),
            mean_Mcap = mean(Mcap),
            mean_N_stocks = n() / length(unique(year))) %>%
  ungroup() %>%
  drop_na()


# 3 年次ポートフォリオを月次データに統合 ----------------------------------------------

# ＜ポイント＞
# - 分位ポートのデータを月次データに1年間適用してリターン分析を行う


# 年次データの加工
# --- Bが含まれるほうがmean_Mcapが大きい
# --- Hが含まれるほうがmean_Bpが大きい
annual_portfolio <-
  annual_tiled_data %>%
    select(year, firm_ID, FF_port_type, Mcap_tile2, Bp_rank3, w)

# 月次データに統合
# --- 月次リターンデータに年次ポートフォリオを統合
FF_portfolio <-
  monthly_data %>%
    full_join(annual_portfolio, by = c("year", "firm_ID")) %>%
    group_by(month_ID, FF_port_type) %>%
    summarize(Mcap_tile2 = first(Mcap_tile2),
              Bp_rank3 = first(Bp_rank3),
              R = sum(w * R, na.rm = TRUE),
              Rf = mean(Rf)) %>%
    ungroup() %>%
    drop_na()


# 4 分位ポートフォリオの平均リターン ------------------------------------------

# ＜ポイント＞
# - バリュー株および小型株の方が超過リターンが出ている
#   --- 超過リターンは対キャッシュリターンのリスクプレミアムである点に注意


# 平均リターンの計算
# --- 各ポートフォリオの超過リターンの平均値を計算
FF_portfolio_mean_return <-
  FF_portfolio %>%
    mutate(Re = R - Rf) %>%
    group_by(FF_port_type) %>%
    summarize(Mcap_tile2 = Mcap_tile2[1],
              Bp_rank3 = Bp_rank3[1],
              mean_Re = mean(Re))

# プロット作成
# --- Size（1：小型株  2：大型株）
# --- Bp（1：グロース株  2：ニュートラル株 3：バリュー株）
FF_portfolio_mean_return %>%
  ggplot(aes(x = Bp_rank3, y = mean_Re, fill = Mcap_tile2)) +
  geom_col(aes(x = Bp_rank3, y = mean_Re, fill = Mcap_tile2), position = "dodge") +
  scale_fill_grey() +
  geom_text(aes(x = Bp_rank3, y = mean_Re, group = Mcap_tile2, label = FF_port_type),
            vjust = -0.5,  position = position_dodge(width = 0.9)) +
  labs(x = "BE/ME Rank", y = "Mean Monthly Excess Return", fill = "ME Rank") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.015)) +
  theme_tq()


# 5 分位ポートフォリオのリターン推移 -------------------------------------------

# 累積リターンの起点を定義
initial_point <-
  tibble(month_ID = c(12, 12),
         cumulative_gross_R = c(1, 1),
         FF_port_type = c("BL", "SH"))

# 累積リターンの作成
# --- initial_pointを第1行に挿入
FF_portfolio_cumulative_return <-
  FF_portfolio %>%
    group_by(FF_port_type) %>%
    mutate(cumulative_gross_R = cumprod(1 + R)) %>%
    ungroup() %>%
    filter(FF_port_type %in% c("BL", "SH")) %>%
    select(month_ID, cumulative_gross_R, FF_port_type) %>%
    bind_rows(initial_point, .)

# プロット作成
FF_portfolio_cumulative_return %>%
  ggplot(aes(x = month_ID, y = cumulative_gross_R, fill = FF_port_type, , color = FF_port_type)) +
  geom_line() +
  scale_linetype_manual(values = c("longdash", "solid")) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "Month ID", y = "Cumulative Gross Return", linetype = "") +
  scale_x_continuous(expand = c(0, 0)) +
  theme_classic()


# 6 スプレッドリターン(SMBとHML)の作成 -----------------------------------------

# ＜ポイント＞
# - 伝統的なFF3モデルは分位レベルでスプレッドリターンを計算してファクターとして扱う
#   --- 実務では｢マーケット｣｢サイズ｣｢バリュー｣を扱うクロスセクションモデルも｢FF3モデル｣と呼ばれることがある


# SMBとHMLを計算
FF_port_spread <-
  FF_portfolio %>%
    pivot_wider(id_cols = month_ID, names_from = FF_port_type, values_from = R) %>%
    mutate(SMB = (SH + SN + SL) / 3 - (BH + BN + BL) / 3,
           HML = (SH + BH) / 2 - (SL + BL) / 2) %>%
    select(month_ID, SMB, HML)

# 3ファクターの実現値をfactor_dataに集約
model_data <-
  factor_data %>%
    full_join(FF_port_spread, by = "month_ID") %>%
    select(-c("SMB", "HML"), c("SMB", "HML"))

 # 3ファクターの実現値をME_sorted_portfolioに追加
ME_sorted_portfolio <-
  ME_sorted_portfolio %>%
    full_join(model_data, by = "month_ID")

# データ確認
ME_sorted_portfolio %>% print()


# 7 FF3のモデル構築 --------------------------------------------------------------

# ＜ポイント＞
# - 分位ごとの超過リターンをR_Me/とSMB/HMLを説明変数として分位ごとに回帰する
#   --- R_Me：マーケット(対キャッシュ超過リターン)
#   --- SMB ：サイズ
#   --- HML ：バリュー


# FF3の実行
# --- 分位ごとにグループ化して回帰
# --- 各モデルの回帰係数の抽出
FF3_results <-
  ME_sorted_portfolio %>%
    group_by(ME_rank10) %>%
    nest() %>%
    mutate(FF3_regression = map(data, ~ lm(Re ~ R_Me + SMB + HML, data = .)),
           FF3_summary = map(FF3_regression, tidy)) %>%
    select(-c(data, FF3_regression)) %>%
    unnest(cols = FF3_summary) %>%
    ungroup()

# 確認
FF3_results %>% print()


# 8 FF3アルファの有意性評価 -----------------------------------------------------------

# - FF3アルファではサイズ相関がなくなり、統計的有意なアルファも減少している
#   --- CAPMではサイズに比例してCAPMアルファが観測された
#   --- FF3ではSMBがサイズに起因するアルファを説明した


# FF3アルファの可視化
# --- 定数項に関する推定結果のみを抽出
FF3_results %>%
  filter(term == "(Intercept)") %>% #
  mutate(ME_rank10 = as.factor(ME_rank10)) %>%
  ggplot(aes(x = ME_rank10, y = estimate)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  labs(x = "ME Rank", y = "FF3 alpha") +
  scale_y_continuous(limits = c(-0.003, 0.013)) +
  theme_tq()


# FF3アルファの統計的な有意性を評価
FF3_results %>%
  filter(term == "(Intercept)") %>%
  rename(FF3_alpha = estimate, p_value = p.value) %>%
  mutate(significance = cut(p_value,
                            breaks = c(0, 0.01, 0.05, 0.1, 1),
                            labels = c("***", "**", "*", ""),
                            include.lowest = TRUE)) %>%
  select(ME_rank10, FF3_alpha, p_value, significance)


# ch06_42: データの保存
#write_csv(factor_data, "data/ch06_output.csv")
