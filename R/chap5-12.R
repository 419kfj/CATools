#
# 第５章（解説 第１２章）
#

# 対数線形モデル解析を行うために、3元クロス表を作成する。

collist <- c("A16.25","A26.35","A36.45","A46.55","A56.66","A67.100")
collistm <- c("m16.25","m26.35","m36.45","m46.55","m56.66","m67.100")
collistf <- c("f16.25","f26.35","f36.45","f46.55","f56.66","f67.100")

rowlist <- c("MTW","OW","M23","OM","MS","never")
rowlist2 <- c("Many times a week","Once a week","2-3times a month","Once a month","More seldom","Never")

f <- matrix(c(22,19,83,69,32,22,
              78,84,130,126,62,19,
              109,120,135,99,40,22,
              108,91,108,50,41,29,
              132,203,160,127,95,62,
              85,90,78,106,126,177),byrow=TRUE,6)

m <- matrix(c(54,65,105,139,82,39,
              134,127,150,149,63,40,
              114,136,124,103,54,42,
              101,87,67,71,44,36,
              104,81,113,88,84,72,
              80,35,38,49,53,100),byrow=TRUE,6)

tb2<- array(c(f,m),
           dim=c(6,6,2),
           dimnames = list(
             ALC = rowlist, #.dd[1:6,2],
             AGE = collist, #colnames(.dd[3:8]),
             SEX = c("female","male")
             ))

tb2.freq <- data.frame(ftable(tb2)) # tableから度数(Freq)データへ
.df5.1.ko <- expand.dft(tb2.freq)
(.tbl5.1 <- with(.df5.1.ko, table(ALC,AGE,SEX))) # 確認！

.tbl5.1wide <- cbind(f,m)
rownames(.tbl5.1wide) <- rowlist2
colnames(.tbl5.1wide) <- c(collistf,collistm)
.tbl5.1wide

.tbl5.1a <- aperm(.tbl5.1,c(2,1,3))
str(.tbl5.1a)# ALC, AGE, SEX ---- S:SEX , A:AGE, C:ACL
dimnames(.tbl5.1a)
summary(.tbl5.1a)
TBL5.1 <- structable(.tbl5.1a)

#
# 対数線形モデル解析 
#

.tbl5.1b <-  .tbl5.1a[,, 2:1]# 2:１ で男女(male/female)としている(原著とあわせた)

library(MASS)
 (res.ind <- loglm(~ SEX + AGE + ALC, data = .tbl5.1b))
(res.base <- loglm(~ SEX * AGE + ALC, data = .tbl5.1b))
(res.M1 <- loglm(~ SEX * ALC + SEX * AGE, data = .tbl5.1b))
(res.M2 <- loglm(~ SEX * ALC + AGE * ALC, data = .tbl5.1b))
(res.M3 <- loglm(~ SEX * ALC + AGE + ALC, data = .tbl5.1b))
(res.M4 <- loglm(~ SEX * ALC + AGE * ALC + SEX * AGE, data = .tbl5.1b))
(res.sat  <- loglm(~ SEX * AGE * ALC, data = .tbl5.1b))

# デモルのあてはまり度合いをmosaicplot で確認
library(vcdExtra)
plot(res.ind,dir=c("v","h","v"),main="独立[S][A][C]",sub=res.ind$formula) 
plot(res.base,dir=c("v","h","v"),main="Base[SA][C]",sub=res.base$formula) 
plot(res.M1,dir=c("v","h","v"),main="M1[SC][SA]",sub=res.M1$formula) 
plot(res.M2,dir=c("v","h","v"),main="M2[SA][AC]",sub=res.M2$formula) 
plot(res.M3,dir=c("v","h","v"),main="M3[SC][AC]",sub=res.M3$formula) 
plot(res.M4,dir=c("v","h","v"),main="M4[SC][AC][SA]",sub=res.M4$formula) 
plot(res.sat,dir=c("v","h","v"),main="独立[SAC]",sub=res.sat$formula)

# 飽和モデルのパラメータを確認
coef(res.sat)

#
# 対応分析を行う
#

library(FactoMineR)
.tbl5.1wj <- .tbl5.1wide
rownames(.tbl5.1wj) <- rname
colnames(.tbl5.1wj)

res.CA <- CA(.tbl5.1wj,graph=FALSE)
plot(res.CA,title="表5.1に対する対応分析")

summary(res.CA,nbelements = 12)
lines(res.CA$col$coord[1:6,1:2],lty=2,col=1)
lines(res.CA$col$coord[7:12,1:2],lty=3,col=2)

#
# 基本統計量を図示する
#
source("CATools.R")

draw_screeplot(res.CA)# スクリープロット
disp_contr(res.CA)# 絶対的寄与
disp_corr2(res.CA)# 平方相関
qty_cos2(res.CA) # 質

