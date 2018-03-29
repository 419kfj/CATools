#
#
#

# 表4.1 を作成する

.tbl <- matrix(c(139,40,40,41,
                 132,42,37,53,
                 131,21,16,15,
                 124,51,64,124,
                 101,45,49,62,
                 15,79,5,4,
                 20,98,34,29,
                 24,47,1,2,
                 5,42,10,1,
                 7,65,12,6,
                 137,114,106,159,
                 61,67,115,62,
                 95,44,83,86,
                 143,83,121,149,
                 57,97,92,98,
                 76,32,56,195,
                 75,49,43,194,
                 63,45,38,171,
                 48,46,18,143,
                 49,113,46,105,
                 111,11,76,95,
                 21,38,23,34,
                 24,36,33,60,
                 115,50,66,106,
                 64,90,66,69,
                 24,8,15,72,
                 71,26,40,58,
                 57,32,29,82,
                 72,53,55,65,
                 30,37,37,52,
                 25,44,18,28,
                 86,15,66,149),byrow=T,32,4)

dimnames(.tbl) <- list(variable=c("精神疾患",#"nervous",
                                  "不健康",#"badhealth",
                                  "鎮痛剤使用",#"sedatives",
                                  "年金（病人、失業者向け）",#"insurance",
                                  "低学歴",#"lowed",
                                  "飲酒",#"alcocons",
                                  "受刑者",#"convicted",
                                  "飲酒に問題",#"alcoprobl",
                                  "麻薬常用",#"narcotics",
                                  "罰金負債",#"penalty",
                                  "日常的喫煙",#"smokes",
                                  "長期患者",#"longterm",
                                  "日常支出に問題",#"dailyexp",
                                  "NOK2000問題",#"nok2000",NOK2000問題
                                  "失業",#"unemployed",
                                  "住宅ローン",#"housedebt",
                                  "持ち家",#"dwelling",
                                  "自家用車所持",#"car",
                                  "高収入",#"income",
                                  "男性",#"men",
                                  "女性",#"women",
                                  "18〜24歳",#"A18.24",
                                  "25〜30歳",#"A25.30", 25-29 ?
                                  "30〜50歳",#"A30.50",
                                  "未婚",#"unmarried",
                                  "既婚",#"married",
                                  "離婚",#"divorced",
                                  "地方",#"rural",
                                  "都会",#,"urban",
                                  "都市",#"city",
                                  "子供なし世帯",#"ChildNot",
                                  "子供あり世帯"),#"Child"),
                       status=c("病気",#"SICK",#病気
                                "逸脱者",#"DEVIANTS",#逸脱者
                                "中毒者",#"DEPENDENTS",#中毒者
                                "負債者"　#"INDEBTED"#負債者
                                )
                       )

#
# 対応分析を行う
#
library(FactoMineR)
res.CA <- CA(.tbl,graph = FALSE)
plot(res.CA,title = "図4.1 生活保護受給者の社会空間")

summary(res.CA,nbelements = 32)

#
# 基本統計量を図示する
#
source("CATools.R")

draw_screeplot(res.CA)# スクリープロ
disp_contr(res.CA)# 絶対的寄与
disp_corr2(res.CA)# 平方相関
qty_cos2(res.CA) # 質

###


         