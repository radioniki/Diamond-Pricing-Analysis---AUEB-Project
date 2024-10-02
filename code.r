# read the data

data<-read.table("E:/02_diamonds.csv",sep=",")
names(data)<-c("indo","carat","colour","clarity","body","price")
head(data)
str(data)
data$price<-as.numeric(data$price)
dim(data)

#normality check for quantitative variable: "diamond price"

qqnorm(data$price,main="normal qq plot for price")
qqline(data$price,col=2)
shapiro.test(data$price)#oxi kanoniko digma#

#normality check for quantitative variable: "diamond size"

qqnorm(data$carat,main="normal qq plot for carat")
qqline(data$carat,col=2)
shapiro.test(data$carat)#oxi kanoniko digma#

#Histograms and boxplots 
par(mfrow=c(2,1))
hist(data$price)
boxplot(data$price)
par(mfrow=c(2,1))
hist(data$carat)
boxplot(data$carat)

#Violin charts

install.packages("sm")
library(sm)
install.packages("vioplot")
library(vioplot)
vioplot(data$price,names="price")
vioplot(data$carat,names="carat")

#Descriptive measures

install.packages("psych")
library(psych)
round(t(describe(data$price)),1)
round(t(describe(data$carat)),1)
ΕΝΤΟΛΗ 6:Κατηγοροποίηση ποσοτικής μεταβλητής καράτια
data$car<-1*(data$carat<0.5)+2*(data$carat>=0.5&data$carat<1)+3*(data$carat>=1)
data$car<-as.factor(data$car)

#pies
par(mfrow=c(2,2))
pie(table(data$car))
title("ΚΑΤΗΓΟΡΙΕΣ ΜΕΓΕΘΟΥΣ")
pie(table(data$colour))
title("ΚΑΤΗΓΟΡΙΕΣ ΧΡΩΜΑΤΟΣ")
pie(table(data$clarity))
title("ΚΑΤΗΓΟΡΙΕΣ ΔΙΑΦΑΝΕΙΑΣ")
pie(table(data$body))
title("ΚΑΤ. ΟΡΓ. ΠΙΣΤΟΠΟΙΗΣΗΣ")

#Bar charts

par(mfrow=c(2,2))
barplot(round(prop.table(table(data$car)),2),col=3)
title("ΚΑΤΗΓΟΡΙΕΣ ΜΕΓΕΘΟΥΣ")
barplot(round(prop.table(table(data$colour)),2),col=3)
title("ΚΑΤΗΓΟΡΙΕΣ ΧΡΩΜΑΤΟΣ")
barplot(round(prop.table(table(data$clarity)),2),col=3)
title("ΚΑΤΗΓΟΡΙΕΣ ΔΙΑΦΑΝΕΙΑΣ")
barplot(round(prop.table(table(data$body)),2),col=3)
title("ΚΑΤ. ΟΡΓ. ΠΙΣΤΟΠΟΙΗΣΗΣ")

#Frequency tables

table(data$car)
round(prop.table(table(data$car)),2)
table(data$colour)
round(prop.table(table(data$colour)),2)
table(data$clarity)
round(prop.table(table(data$clarity)),2)
table(data$body)
round(prop.table(table(data$body)),2)

#Relevance tables

tab1<-table(data$colour,data$clarity)
install.packages("gmodels")
library(gmodels)
CrossTable(data$colour,data$clarity)
tab2<-table(data$colour,data$car)
CrossTable(data$colour,data$car)
tab3<-table(data$colour,data$body)
CrossTable(data$colour,data$body)
tab4<-table(data$clarity,data$body)
CrossTable(data$clarity,data$body)
tab5<-table(data$car,data$body)
CrossTable(data$car,data$body)
tab6<-table(data$clarity,data$car)
CrossTable(data$clarity,data$car)

#Stacked and grouped bar charts

mycol1<-1:length(levels(data$colour))
mycol2<-1:length(levels(data$clarity))
mycol3<-1:length(levels(data$car))
barplot(tab1,beside=T,col=mycol1,legend=levels(data$colour),xlab="ΔΙΑΦΑΝΕΙΑ")
barplot(tab1,beside=F,col=mycol1,legend=levels(data$colour),xlab="ΔΙΑΦΑΝΕΙΑ")
title("ΣΧΕΣΗ ΧΡΩΜΑΤΟΣ ΔΙΑΦΑΝΕΙΑΣ")
barplot(tab2,beside=T,col=mycol1,legend=levels(data$colour),xlab="ΜΕΓΕΘΟΣ ΠΕΤΡΑΣ") barplot(tab2,beside=F,col=mycol1,legend=levels(data$colour),xlab="ΜΕΓΕΘΟΣ ΠΕΤΡΑΣ")
title("ΣΧΕΣΗ ΧΡΩΜΑΤΟΣ ΜΕΓΕΘΟΥΣ")
barplot(tab3,beside=T,col=mycol1,legend=levels(data$colour),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab3,beside=F,col=mycol1,legend=levels(data$colour),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ"
title("ΣΧΕΣΗ ΧΡΩΜΑΤΟΣ ΟΡΓΑΝΙΣΜΟΥ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab4,beside=T,col=mycol2,legend=levels(data$clarity),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab4,beside=F,col=mycol2,legend=levels(data$clarity),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ")
title("ΣΧΕΣΗ ΔΙΑΦΑΝΕΙΑΣ ΟΡΓΑΝΙΣΜΟΥ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab5,beside=T,col=mycol3,legend=levels(data$car),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab5,beside=F,col=mycol3,legend=levels(data$car),xlab="ΟΡΓΑΝΙΣΜΟΣ ΠΙΣΤΟΠΟΙΗΣΗΣ")
title("ΣΧΕΣΗ ΜΕΓΕΘΟΥΣ ΟΡΓΑΝΙΣΜΟΥ ΠΙΣΤΟΠΟΙΗΣΗΣ")
barplot(tab6,beside=T,col=mycol2,legend=levels(data$clarity),xlab="ΜΕΓΕΘΟΣ")
barplot(tab6,beside=F,col=mycol2,legend=levels(data$clarity),xlab="ΜΕΓΕΘΟΣ")
title("ΣΧΕΣΗ ΔΙΑΦΑΝΕΙΑΣ ΜΕΓΕΘΟΥΣ")

#x2 controls

chisq.test(data$colour,data$clarity)
chisq.test(data$colour,data$car)
chisq.test(data$colour,data$body)
chisq.test(data$clarity,data$body)
chisq.test(data$car,data$body)
chisq.test(data$clarity,data$car)
ΕΝΤΟΛΗ 13:Έλεγχος αν το προϊόν είναι ακριβό
wilcox.test(data$price,alternative="greater",mu=4000)

#Price-transparency relationship

summary(aov(price~clarity,data=data))
shapiro.test(aov(price~clarity,data=data)$res)#oxi kanonika katalipa#
leveneTest(price~clarity,data=data,center=mean)#oxi omoskedastika katalipa#
#megalo digma kano elegxo gia tin isotita ton diameson#
kruskal.test(price~clarity,data=data)#aporipto Ho ara iparxi diafora#
boxplot(data$price~data$clarity)

#Price-transparency relationship
#sxesi timis me diafania#
summary(aov(price~clarity,data=data))
shapiro.test(aov(price~clarity,data=data)$res)#oxi kanonika katalipa#
leveneTest(price~clarity,data=data,center=mean)#oxi omoskedastika katalipa#
#megalo digma kano elegxo gia tin isotita ton diameson#
kruskal.test(price~clarity,data=data)#aporipto Ho ara iparxi diafora#
boxplot(data$price~data$clarity)
title("ΣΧΕΣΗ ΤΙΜΗΣ ΚΑΙ ΔΙΑΦΑΝΕΙΑΣ")

#Price relationship - certification body

#sxesi timis me organismo pistopiisis#
summary(aov(price~body,data=data))
shapiro.test(aov(price~body,data=data)$res)#oxi kanonika katalipa#
leveneTest(price~body,data=data,center=mean)#oxi omoskedastika katalipa#
#megalo digma kano elegxo mi parametriko gia tin isotita ton diameson#
kruskal.test(price~body,data=data)# aporipto Ho ara iparxi diafora#
boxplot(data$price~data$body)

#Price-color relationship
#sxesi timis me xroma#
summary(aov(price~colour,data=data))
shapiro.test(aov(price~colour,data=data)$res)#oxi kanonika katalipa#
leveneTest(price~colour,data=data,center=mean)#oxi omoskedastika katalipa#
#megalo digma kano elegxo gia tin isotita ton diameson#
kruskal.test(price~colour,data=data)# den aporipto Ho ara den iparxi diafora#
boxplot(data$price~data$colour)


#sxesi timis me katigoriki metabliti karatia#
summary(aov(price~car,data=data))
shapiro.test(aov(price~car,data=data)$res)#oxi kanonika katalipa#
leveneTest(price~car,data=data,center=mean)#oxi omoskedastika katalipa#
#megalo digma kano elegxo mi parametriko gia tin isotita ton diameson#
kruskal.test(price~car,data=data)#aporipto Ho ara iparxi diafora#
boxplot(data$price~data$car)

#Correlation coefficients
cor(data$price,data$carat)
cor(data$price,data$carat,method="kendal")
cor(data$price,data$carat,method="spearman")


plot(data$carat,data$price)
abline(lm(data$price~data$carat),col=2)


l1<-lm(data$price~data$carat)
summary(l1)
#elegxos kanonikotitas katalipon#
qqnorm(l1$res)
qqline(l1$res)
shapiro.test(l1$res)#oxi kanonika katalipa#
lillie.test(l1$res)#oxi kanonika katalipa#

#I'm trying transformations
#dokimazo metasximatismous#
l2<-lm(log(data$price)~data$carat)
l3<-lm(log(data$price)~log(data$carat))
l4<-lm(data$price~log(data$carat))
l5<-lm(I(1/data$price)~data$carat)
l6<-lm(data$price~I(1/data$carat))
l7<-lm(I(1/data$price)~I(1/data$carat))
l8<-lm(data$price~I(data$carat-mean(data$carat)))
shapiro.test(l2$res)
shapiro.test(l3$res)
shapiro.test(l4$res)
shapiro.test(l5$res)
shapiro.test(l6$res)
shapiro.test(l7$res)
shapiro.test(l8$res)
lillie.test(l2$res)
lillie.test(l3$res)


#elegxo ipolipes proipothesis gia l2 #
#elegxos omoskedastikotitas katalipon# ###Omoskedastikotha Kataloipwn#####
##rstandard vs X##,##rstandard vs Yhat##,##st.res.sq vs Yhat##
#res vs X
plot( data$carat, rstandard(l2) , ylim=c(-2.2,2.2), pch=16, cex=2, col='blue')
abline(h=1.96, col='red', lwd=2, lty=2)
abline(h=-1.96, col='red', lwd=2, lty=2)
#st.res vs Yhat
plot( l2$fit, rstandard(l2) , ylim=c(-2.2,2.2), pch=16, cex=2, col='blue')
abline(h=1.96, col='red', lwd=2, lty=2)
abline(h=-1.96, col='red', lwd=2, lty=2)
# Elegxos isothtas Diakumansewn ana tetarthmorio ths X #
x<-cut(data$carat-mean(data$carat), breaks= quantile(data$carat-mean(data$carat)),include.lowest=T)
table(x)
var.test(rstandard(l2),x)


#elegxos anejartisias katalipon#
dwt(l2)
durbinWatsonTest(l2)
durbinWatsonTest(l2,max.lag=7)

# Matasxhmatismoi Boc Cox#
library(MASS)
boxcox(l1, lambda=seq(-5,5,0.1) )
temp<-boxcox(l1, lambda=seq(-5,5,0.1) )
temp$x[ temp$y==max(temp$y) ]

#polionimiki palindromisi#
hp<-lm(price~poly(carat,6,raw=T),data=data)
summary(hp)
AIC(hp)
BIC(hp)

###Akraies times#
#plot Yhat vs st.res#
par(bty='l')
plot(data$carat-mean(data$carat), rstandard(l1) , ylim=c(-2.2,2.2), pch=16, cex=2, col='blue')
abline(h=1.96, col='red', lwd=2, lty=2)
abline(h=-1.96, col='red', lwd=2, lty=2)

#Shmeia ephrrohs#

plot( l1, pch=16, cex=2, col='blue', add.smooth=T, which=3)
#cook distance
plot( l1, pch=16, cex=2, col='blue', which=4)
abline(h=4/(1078-1-1), col='red', lty=2, lwd=2)###empeiriko shmeio diaxwrismou 4/(n-p-1)#
plot( l1, pch=16, cex=2, col='blue', which=5)
plot( l1, pch=16, cex=2, col='blue', which=6)
ΕΝΤΟΛΗ 29:Αναζήτηση μοντέλου
anova1<-lm(price~car+colour+clarity+body,data=data)
leveneTest(anova1$res~data$car*data$colour*data$body*data$clarity)
shapiro.test(anova1$res)
lillie.test(anova1$res)

#Comparison of constant with full model

null<-aov(price~1,data=data)
main.effects<-aov(price~car+colour+body+clarity,data=data)
anova(null,main.effects)
#Kanonikothta#
shapiro.test(main.effects$res)
lillie.test(main.effects$res)
#testing models

#dokimazo me 2 h 3 katigorikes#
a2<-lm( price~car+body, data=data)
a3<-lm( price~colour+car, data=data)
a4<-lm( price~clarity+car, data=data)
a5<-lm( price~body+clarity, data=data)
a6<-lm( price~body+colour, data=data)
a7<-lm( price~clarity+colour, data=data)
a8<-lm( price~car+body+colour, data=data)
a9<-lm( price~car+body+clarity, data=data)
a10<-lm( price~colour+body+clarity, data=data)
a11<-lm( price~colour+car+clarity, data=data)
shapiro.test(a2$res)
shapiro.test(a3$res)
shapiro.test(a4$res)
shapiro.test(a5$res)
shapiro.test(a6$res)
shapiro.test(a7$res)
shapiro.test(a8$res)
shapiro.test(a9$res)
shapiro.test(a10$res)
shapiro.test(a11$res)

#testing models with interactions

#dokimazo me allilepidraseis#
a12<-lm(price~car*colour,data=data)
shapiro.test(a12$res)
a13<-lm(price~car*clarity,data=data)
shapiro.test(a13$res)
a14<-lm(price~car*body,data=data)
shapiro.test(a14$res)
a15<-lm(price~clarity*body,data=data)
shapiro.test(a15$res)
a16<-lm(price~clarity*colour,data=data)
shapiro.test(a16$res)
a17<-lm(price~car*colour+car*clarity+car*body+clarity*body+clarity*colour+body*colour,data=data)
shapiro.test(a17$res)

#model found

a18<-lm(price~car+body+colour+clarity+colour*clarity,data=data)
shapiro.test(a18$res)
lillie.test(a18$res)
qqnorm(a18$res)
qqline(a18$res)
leveneTest(a18$res~data$car*data$colour*data$clarity*data$body)

#Model selection with AIC

mnull<-lm(price~1,data=data)
summary(step(mnull, scope=list(lower=mnull,upper=a18), direction='both'))
summary(step(mnull, scope=list(lower=mnull,upper=a18), direction='forward'))
ΕΝΤΟΛΗ 35:Αφαίρεση ακραίων τιμών
data1<-data[-c(131,116,279),]
l<-lm(data1$price~data1$carat)
par(mfrow=c(2,2))
plot( l, pch=16, cex=2, col='blue', which=4)
data2<-data1[-c(291,276,119),]
n<-lm(data2$price~data2$carat)
plot( n, pch=16, cex=2, col='blue', which=4)
data3<-data2[-c(289,277,291),]
k<-lm(data3$price~data3$carat)
plot( k, pch=16, cex=2, col='blue', which=4)
data4<-data3[-c(110,116,275),]
v<-lm(data4$price~data4$carat)
plot( v, pch=16, cex=2, col='blue', which=4)
data5<-data4[-c(118,139,293),]

#Model with new data set and condition checks

anova1<-lm(price~car+colour+body+clarity,data=data5)
leveneTest(anova1$res~data5$car*data5$colour*data5$body*data5$clarity)
shapiro.test(anova1$res)
lillie.test(anova1$res)
summary(anova1)