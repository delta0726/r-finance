# ***********************************************************************************************
# Title   : Financial Risk Modeling and Portfolio Optimization with R
# Chapter : 2 A bried course in R
# Theme   : 5 The accompanying package FRAPO
# Date    : 2022/10/29
# Page    : P22 - P28
# URL     : https://www.pfaffikus.de/rpacks/
# ***********************************************************************************************


# ＜概要＞
# - 当書籍は{FRAPO}という専用ライブラリを提供してくれている


# ＜目次＞
# 0 準備
# 1 サンプルコード
# 2 サンプルデータ


# 0 準備 ----------------------------------------------------------------------

# インストール
# install.packages("FRAPO")


# ライブラリ
library(tidyverse)
library(FRAPO)


# ヘルプ
help(package = FRAPO)

# 関数検索
?BookEx


# 1 サンプルコード --------------------------------------------------------------

# サンプル一覧
listEx()

# サンプルコード
showEx("C3R1")

# サンプルコードの実行
runEx("C3R1", echo = TRUE)


# 2 サンプルデータ --------------------------------------------------------------

# ESCB FX Reference Data
data(ESCBFX)
ESCBFX %>% as_tibble()

# Euro Stoxx50
data(EuroStoxx50)
EuroStoxx50 %>% as_tibble()

# FTSE100
data(FTSE100)
FTSE100 %>% as_tibble()

# Hang Seng Index and Consttuents
data(INDTRACK1)
INDTRACK1 %>% as_tibble()

# DAX100 Index and Consttuents
data(INDTRACK2)
INDTRACK2 %>% as_tibble()

# FTSE100 Index and Consttuents
data(INDTRACK3)
INDTRACK3 %>% as_tibble()

# Standard & Poor's 100 Index and Consttuents
data(INDTRACK4)
INDTRACK4 %>% as_tibble()

# Nikkei225 Index and Consttuents
data(INDTRACK5)
INDTRACK5 %>% as_tibble()

# Standard & Poor's 500 Index and Consttuents
data(INDTRACK6)
INDTRACK6 %>% as_tibble()

# Milano Indice Borsa Telematica
data(MIBTEL)
INDTRACK6 %>% as_tibble()

# Multi Asset Index Data
data(MultiAsset)
INDTRACK6 %>% as_tibble()

# NASDAQ
data(NASDAQ)
NASDAQ %>% as_tibble()

# Standard & Poor's 500
data(SP500)
SP500 %>% as_tibble()

# Stock Index Data
data(StockIndex)
StockIndex %>% as_tibble()

# Stock Index Data (Month-End)
data(StockIndexAdj)
StockIndexAdj %>% as_tibble()

# Stock Index Data (Daily)
data(StockIndexAdjD)
StockIndexAdjD %>% as_tibble()

