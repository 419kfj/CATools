#
# 表2.1 を作成する
#

.tbl2.1 <- matrix(c(395, 2456,1758,4609, 749, 66,
                 147, 153, 916, 1216, 235 , 135,
                 694, 327, 1347,2368, 283,185,
                 1236,2936, 4021, 8193, 1131, 297,
                 4558, 5129, 10842, 20529,NA, NA),byrow=T,5,6)
dimnames(.tbl2.1) <- list(地域=c("オスロ","中部地域","北部地域",
                              "合計","全国"),
                       犯罪=c("強盗", "詐欺","破壊","合計", 
                               "密","疎") )
.tbl2.1
.tbl2.1a <- .tbl2.1[1:3,1:3]# 地域3つ、犯罪３種類の部分を抽出
.tbl2.1a

# 
# 対応分析を行う FactoMineR
# 
library(FactoMineR)
res.CA <- CA(.tbl2.1a) # これでグラフまで描いてくれます。
summary(res.CA)

# CATools.R を読み込んでおけば以下の処理が可能です。
source("CATools.R") # 本スクリプトと同じところにおいてください。

draw_screeplot(res.CA) # スクリープロット
disp_contr(res.CA) # 絶対的寄与
disp_corr2(res.CA) # 平方相関
qty_cos2(res.CA) # 質 

#
# サプリメタリー・ポイントを指定する
#

# データ表を作成
(.tbl2.1s <- .tbl2.1[c(1:3,5),c(1:3,5,6)])　
res.CA.sup <- CA(.tbl2.1s,row.sup=4,col.sup = c(4,5),graph=FALSE)
plot(res.CA.sup,title="表2.1を対応分析。サプリ・ポイント付き")
lines(res.CA.sup$col.sup$coord,lty=2)# 破線（lty＝２）で、密と疎をつなぎます

###