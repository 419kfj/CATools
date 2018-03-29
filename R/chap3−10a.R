#
# 第３章（解説は第１０章）
#
# 2017/08/04 type 修正 L57 ◯Type <- rname × Type <- rnames 

# 表3.1 を作る

cname <- c("A0.6", "A7.15", "A16.24", "A25.44", "A45.66", "A67." )#A=Age
rname <- c("精神障害",#"Nervous disorder",
                   "神経系",#"Nervous system",
                   "耳鼻科",#"Eye and ear",
                   "循環器系",#"Cardiovascular",
                   "呼吸器系",#"Respiatory ograns",
                   "胃潰瘍",#"Stomach ulser",
                   "他の消化器系疾患",#"Other digestive dis",
                   "泌尿器/生殖器",#"Urinary/genital system",
                   "上皮皮下組織",#"Skin and subcutis",
                   "筋骨格疾患",#"Musculoskeletal dis",
                   "他の疾患",#"Other diseases",
                   "怪我"#"Injuries"
                    )

.tbl3.1 <- matrix(c(12, 7, 44, 12, 63,0, 9, 8, 103, 22,30, 7, # 列方向に読んでます
                    22, 11, 47,6, 70, 1, 5, 9,110, 43, 42, 13,
                    35,35, 45, 5, 69, 8,5, 30, 138, 105, 42,38,
                    68, 45, 42, 38,74, 14, 15, 28, 124,165, 72, 32,
                    102, 49,68, 222, 80, 36, 32,39, 88, 314, 126, 48,
                    147, 33, 155, 469, 84,37, 64, 56, 54, 334,235, 76),
                  byrow=FALSE,12,6)
dimnames(.tbl3.1) <- list(Type=rname,
                          Age=cname)

# 対応分析を行う

library(FactoMineR)
res.CA <- CA(.tbl3.1,graph=FALSE)

summary(res.CA,nbelements = 12)#nbelementsのdefault は10
plot(res.CA,title="表3.1")

#
# 基本統計量を図示する
#
source("CATools.R")

draw_screeplot(res.CA)# スクリープロ
disp_contr(res.CA)# 絶対的寄与
disp_corr2(res.CA)# 平方相関
qty_cos2(res.CA) # 質

#
# 表3.4 三元クロス表をつくる
#

sex <-c("Men","Women")
Age <- c("A0.6", "A7.15", "A16.24", "A25.44", "A45.66", "A67." )
Type <- rname
m <- matrix(c(8,25,23,46,66,106, # 男性データ
              4,13,30,29,29,31,
              38,52,41,48,72,156,
              10,7,5,39,226,399,
              70,77,72,77,89,100,
              0,0,8,22,43,43,
              12,3,3,16,21,60,
              10,11,11,12,22,72,
              105,106,119,106,72,55,
              12,36,95,153,267,266,
              38,47,41,52,101,192,
              4,13,55,46,63,77))#,byrow=T,12,6)
f <- matrix(c(17,19,45,89,135,179, # 女性データ
              10,9,41,60,68,34,
              50,44,48,36,64,155,
              15,5,5,38,218,523,
              56,64,67,70,72,72,
              0,3,8,5,29,33,
              6,6,6,15,42,68,
              6,8,48,45,55,43,
              100,113,156,141,104,54,
              33,49,115,177,358,387,
              21,37,44,92,150,268,
              10,14,23,19,34,74))#,byrow=T,12,6)

library(vcdExtra)
data <- expand.grid(Age=Age,Type=Type,sex=sex)
count <- c(m,f)
data <- cbind(data,count) #
.tbl3.4 <- xtabs(count ~ Type + Age +sex, data = data) 

.tbl3.4.M <- .tbl3.4[,,"Men"]
.tbl3.4.W <- .tbl3.4[,,"Women"]
.tbl3.4MW <- cbind(.tbl3.4.M, .tbl3.4.W)
dimnames(.tbl3.4MW)[[2]] <- c(chartr("A","M",colnames(.tbl3.4.M)),#列名付け替え
                            chartr("A","W",colnames(.tbl3.4.W)))
.tbl3.4MW # 回り道してつくっている。f m から組んだほうが簡単。

res.CA <- CA(.tbl3.4MW,graph = FALSE)
plot(res.CA,axes=c(1,2),title="図3.2 Dim1,2")# Dim1,2を表示
# 性別年齢にlineをひきます
lines(res.CA$col$coord[1:6,1:2],lty=1)
lines(res.CA$col$coord[7:12,1:2],lty=2)

#
# 基本統計量を図示する
#
source("CATools.R")

draw_screeplot(res.CA)# スクリープロ
disp_contr(res.CA)# 絶対的寄与
disp_corr2(res.CA)# 平方相関
qty_cos2(res.CA) # 質

###