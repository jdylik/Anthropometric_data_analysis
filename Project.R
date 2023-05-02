#Ostatecznie zmieni³em temat projektu, w którym za pomoc¹ danych antropometrycznych predykujê poziom masy miêœniowej w nogach, który prawdopodobnie wzrasta przy oty³oœci  
setwd("D:/polsl/IV/bio")
antro = read.csv("dane_antropo_13.csv", header=TRUE, sep=",", dec=".")
inbo = read.csv("dane_inbody.csv", header=TRUE, sep=",", dec=".")

#czyszczenie danych
#antro
#zmieniam typ pierwszej kolumny na faktorowy
#entire_antro to dane antro dla analizy wszystkich cech, antro to dane dla wybranych cech
entire_antro = antro[,-2]
entire_antro[,1] = factor(entire_antro[,1])
antro[,1] = factor(antro[,1])
#zamiana 0 - czyli prawdopodobnie b³êdów urz¹dzeñ lub braku pomiaru - na NA
entire_antro[entire_antro==0]=NA
entire_antro[entire_antro==""]=NA
antro[antro==0]=NA
antro[antro==""]=NA
#jako jedyne pozostawiam kolumny: id, data badania, obwód pasa, bioder, uda, fa³d na brzuchu, ciœnienia i têtno krwi
antro = antro[,c("id", "DataBadania", "ObwodPasa", "ObwodBioder", "ObwodUda", "FaldNaBrzuchu", "CisnienieKrwiSkurczowe", "CisnienieKrwiRozkurczowe", "TetnoKrwi")]
#tutaj edytujê kolumnê daty - zast¹pienie pauz kropkami, a nastêpnie zmiana dat do jednego, spójnego formatu
antro$DataBadania = gsub("-", ".", antro$DataBadania)
entire_antro$DataBadania = gsub("-", ".", entire_antro$DataBadania)
for(j in 1:nrow(antro))
{
  splited = unlist(strsplit(antro[j,2], split="[.]"))
  ent_splited = unlist(strsplit(entire_antro[j,2], split="[.]"))
  if (nchar(splited[1]) != 4)
  {
    day = splited[1]
    month = splited[2]
    year = splited[3]
    date = paste(paste(year, month, sep="."), day, sep=".")
    antro[j,2] = date
    entire_antro[j,2] = date
  }
}

#czyszczenie inbo
#pozostawiam kolumny dotycz¹ce p³ci, wieku oraz fat-free mass obu nóg i ich poprawne przedzia³y
inbo = inbo[, c("id", "DataBadania", "Sex", "Age", "FFMofRightLeg",
                "LowerLimit_FFMofRightLegNormalRange_", "UpperLimit_FFMofRightLegNormalRange_",
                "FFMofLeftLeg", "LowerLimit_FFMofLeftLegNormalRange_", "UpperLimit_FFMofLeftLegNormalRange_")]
inbo[,1] = factor(inbo[,1])
#tutaj problem z dat¹ to jedynie niepotrzebne dane godzin - po prostu je usuwam i zachowujê tylko dzienn¹ datê
inbo$DataBadania = substr(inbo$DataBadania, 1, 10)
#z³¹czenie obu dataframe'ów na dwóch kolumnach
merged = merge(antro, inbo, by=c("id", "DataBadania"))
entire_merged = merge(entire_antro, inbo, by=c("id", "DataBadania"))
#usuwam duplikaty
final = merged[!duplicated(merged),]
#zawê¿am dataset do tych obserwacji, które maj¹ poprawnie obliczone przedzia³y normalnych wartoœci fat-free mass dla nóg
final = final[final$LowerLimit_FFMofLeftLegNormalRange_ < final$UpperLimit_FFMofLeftLegNormalRange_,]
ent_final = entire_merged[!duplicated(entire_merged),]
ent_final = ent_final[ent_final$LowerLimit_FFMofLeftLegNormalRange_ < ent_final$UpperLimit_FFMofLeftLegNormalRange_,]
#tworzê dwie nowe kolumny - w zale¿noœci od tego, czy fat-free mass nóg jest wiêksze od górnej wartoœci granicznej przedzia³u czy nie, w kolumnie znajdzie siê wartoœci 1 (zwiêkszona iloœæ miêœni w nodze) lub 0
for(j in 1:nrow(final))
{
  if (final[j,15] > final[j,17])
  {
    final[j, 18] = 1
  }
  else
  {
    final[j, 18] = 0
  }
}
colnames(final)[18] = "Miesnielewejnogi"
final[,18]=factor(final[,18])
for(j in 1:nrow(final))
{
  if (final[j,12] > final[j,14])
  {
    final[j, 19] = 1
  }
  else
  {
    final[j, 19] = 0
  }
}
colnames(final)[19] = "Miesnieprawejnogi"
final[,19]=factor(final[,19])
summary(final)

for(j in 1:nrow(ent_final))
{
  if (ent_final[j,27] > ent_final[j,29])
  {
    ent_final[j, 30] = 1
  }
  else
  {
    ent_final[j, 30] = 0
  }
}
colnames(ent_final)[30] = "Miesnielewejnogi"
ent_final[,30]=factor(ent_final[,30])
for(j in 1:nrow(ent_final))
{
  if (ent_final[j,24] > ent_final[j,26])
  {
    ent_final[j, 31] = 1
  }
  else
  {
    ent_final[j, 31] = 0
  }
}
colnames(ent_final)[31] = "Miesnieprawejnogi"
ent_final[,31]=factor(ent_final[,31])
summary(ent_final)

#badam korelacjê danych
library(corrplot)
library(PerformanceAnalytics)
do.korelacji.antro=c(3:9)
do.korelacji.inbody=c(12,15)
M=cor(x=final[,do.korelacji.antro],y=final[,do.korelacji.inbody],use="pairwise.complete.obs",method="pearson")
corrplot(M, method="number")

#funkcja s³u¿¹ca do okreœlenia 5 najwa¿niejszych wskaŸników wynikowych modeli i zapisu grafik wynikowych
ocena<<-function(macierz.bledow, nazwa.klasyfikatora="brak nazwy"){
  #macierz postaci |TP FP|
  #                |FN TN|
  tp=macierz.bledow[1,1]
  fp=macierz.bledow[1,2]
  fn=macierz.bledow[2,1]
  tn=macierz.bledow[2,2]
  
  acc=(tp+tn)/(tp+fp+tn+fn)
  sen=tp/(tp+fn)
  spe=tn/(tn+fp)
  pre=tp/(tp+fp)
  f1=2*pre*sen/(pre+sen)
  jakosc=c(acc,sen,spe,pre,f1)
  nazwy=c("dokladnosc","czulosc","specyficznoœæ","precyzja","f1")
  while(names(dev.cur())!="null device")dev.off()
  png(paste(nazwa.klasyfikatora,".png",sep=""),width=1000,height=800)
  barplot(jakosc,main=nazwa.klasyfikatora,names=nazwy,ylim=c(0,1))
  dev.off()
  jakosc.ramka=data.frame(acc,sen,spe,pre,f1)
  return(jakosc.ramka)
}

#TRENOWANIE I TESTOWANIE MODELI
#prawa noga - wszystkie cechy
#knn
library(caTools)
ent_final_to_calc_r = ent_final[,-c(1,2,24:30)]
ent_final_to_calc_r[20] = ifelse(ent_final_to_calc_r$Sex == "M", 1, 0)
ent_final_to_calc_r=ent_final_to_calc_r[complete.cases(ent_final_to_calc_r),]
ent_split_r=sample.split(ent_final_to_calc_r$Miesnieprawejnogi,SplitRatio = 0.8)
ent_train_set_r=subset(ent_final_to_calc_r,split=TRUE)
ent_test_set_r=subset(ent_final_to_calc_r,split=FALSE)
ent_trainX_r=ent_train_set_r[,-22]
ent_testX_r=ent_test_set_r[,-22]
ent_trainY_r=ent_train_set_r[,22]
ent_testY_r=ent_test_set_r[,22]
ent_trainX_s_r=scale(ent_trainX_r)
ent_testX_s_r=scale(ent_testX_r)
library(class)
knn.pred=knn(ent_trainX_s_r, ent_testX_s_r, ent_final_to_calc_r$Miesnieprawejnogi, k=5)
wyn=table(knn.pred,ent_testY_r)
wyn.knn=ocena(wyn,"KNN dla wszystkich cech dla prawej nogi")
print(wyn.knn)

#drzewo decyzyjne
library(rpart)
library(rpart.plot)
classtree=rpart(formula=Miesnieprawejnogi~.,data=ent_train_set_r,method="class",control=rpart.control(maxdepth=3))#5,9
rpart.plot(classtree, box.palette = "RdBu",digit=-2)
pdd.pred=predict(classtree, ent_test_set_r,type="class")
wyn=table(ent_test_set_r$Miesnieprawejnogi,pdd.pred)
wyn.pdd=ocena(wyn,"Drzewo dla wszystkich cech dla prawej nogi")
wyn.pdd

#svm o j¹drze liniowym
library(e1071)
svmfit=svm(Miesnieprawejnogi~.,data=ent_train_set_r, kernel="linear",
           scale=TRUE)
svm_lin_pred=predict(svmfit,ent_test_set_r)
wyn=table(ent_test_set_r$Miesnieprawejnogi,svm_lin_pred)
wyn.svm.lin=ocena(wyn,"SVM liniowy dla wszystkich cech dla prawej nogi")
print(wyn.svm.lin)

#svm o j¹drze wielomianowym
svmfitP=svm(Miesnieprawejnogi~.,data=ent_train_set_r,kernel="polynomial",
            degree=2)
svm.poly.pred=predict(svmfitP,ent_test_set_r)
wyn=table(ent_test_set_r$Miesnieprawejnogi,svm.poly.pred)
wyn.svm.poly=ocena(wyn,"SVM wielomianowy dla wszystkich cech dla prawej nogi")
print(wyn.svm.poly)


#prawa noga - okrojony zestaw cech
#knn
final_to_calc_r = final[,-c(1,2, 12:18)]
final_to_calc_r[8] = ifelse(final_to_calc_r$Sex == "M", 1, 0)
final_to_calc_r=final_to_calc_r[complete.cases(final_to_calc_r),]
split_r=sample.split(final_to_calc_r$Miesnieprawejnogi,SplitRatio = 0.8)
train_set_r=subset(final_to_calc_r,split=TRUE)
test_set_r=subset(final_to_calc_r,split=FALSE)
trainX_r=train_set_r[,-10]
testX_r=test_set_r[,-10]
trainY_r=train_set_r[,10]
testY_r=test_set_r[,10]
trainX_s_r=scale(trainX_r)
testX_s_r=scale(testX_r)
knn.pred=knn(trainX_s_r, testX_s_r, final_to_calc_r$Miesnieprawejnogi, k=5)
wyn=table(knn.pred,testY_r)
wyn.knn=ocena(wyn,"KNN dla wybranych cech dla prawej nogi")
print(wyn.knn)

#drzewo decyzyjne
classtree=rpart(formula=Miesnieprawejnogi~.,data=train_set_r,method="class",control=rpart.control(maxdepth=3))#5,9
rpart.plot(classtree, box.palette = "RdBu",digit=-2)
pdd.pred=predict(classtree, test_set_r,type="class")
wyn=table(test_set_r$Miesnieprawejnogi,pdd.pred)
wyn.pdd=ocena(wyn,"Drzewo dla wybranych cech dla prawej nogi")
wyn.pdd

#svm o j¹drze liniowym
svmfit=svm(Miesnieprawejnogi~.,data=train_set_r, kernel="linear",
           scale=TRUE)
svm_lin_pred=predict(svmfit,test_set_r)
wyn=table(test_set_r$Miesnieprawejnogi,svm_lin_pred)
wyn.svm.lin=ocena(wyn,"SVM liniowy dla wybranych cech dla prawej nogi")
print(wyn.svm.lin)

#svm o j¹drze wielomianowym
svmfitP=svm(Miesnieprawejnogi~.,data=train_set_r,kernel="polynomial",
            degree=2)
svm.poly.pred=predict(svmfitP,test_set_r)
wyn=table(test_set_r$Miesnieprawejnogi,svm.poly.pred)
wyn.svm.poly=ocena(wyn,"SVM wielomianowy dla wszystkich cech dla prawej nogi")
print(wyn.svm.poly)

#lewa noga - wszystkie cechy
#knn
ent_final_to_calc_l = ent_final[,-c(1,2,24:29, 31)]
ent_final_to_calc_l[20] = ifelse(ent_final_to_calc_l$Sex == "M", 1, 0)
ent_final_to_calc_l=ent_final_to_calc_l[complete.cases(ent_final_to_calc_l),]
ent_split_l=sample.split(ent_final_to_calc_l$Miesnielewejnogi,SplitRatio = 0.8)
ent_train_set_l=subset(ent_final_to_calc_l,split=TRUE)
ent_test_set_l=subset(ent_final_to_calc_l,split=FALSE)
ent_trainX_l=ent_train_set_l[,-22]
ent_testX_l=ent_test_set_l[,-22]
ent_trainY_l=ent_train_set_l[,22]
ent_testY_l=ent_test_set_l[,22]
ent_trainX_s_l=scale(ent_trainX_l)
ent_testX_s_l=scale(ent_testX_l)
knn.pred=knn(ent_trainX_s_l, ent_testX_s_l, ent_final_to_calc_l$Miesnielewejnogi, k=5)
wyn=table(knn.pred,ent_testY_l)
wyn.knn=ocena(wyn,"KNN dla wszystkich cech dla lewej nogi ")
print(wyn.knn)

#drzewo decyzyjne
classtree=rpart(formula=Miesnielewejnogi~.,data=ent_train_set_l,method="class",control=rpart.control(maxdepth=3))#5,9
rpart.plot(classtree, box.palette = "RdBu",digit=-2)
pdd.pred=predict(classtree, ent_test_set_l,type="class")
wyn=table(ent_test_set_l$Miesnielewejnogi,pdd.pred)
wyn.pdd=ocena(wyn,"Drzewo dla wszystkich cech dla lewej nogi")
wyn.pdd

#svm o j¹drze liniowym
svmfit=svm(Miesnielewejnogi~.,data=ent_train_set_l, kernel="linear",
           scale=TRUE)
svm_lin_pred=predict(svmfit,ent_test_set_l)
wyn=table(ent_test_set_l$Miesnielewejnogi,svm_lin_pred)
wyn.svm.lin=ocena(wyn,"SVM liniowy dla wszystkich cech dla lewej nogi")
print(wyn.svm.lin)

#svm o j¹drze wielomianowym
svmfitP=svm(Miesnielewejnogi~.,data=ent_train_set_l,kernel="polynomial",
            degree=2)
svm.poly.pred=predict(svmfitP,ent_test_set_l)
wyn=table(ent_test_set_l$Miesnielewejnogi,svm.poly.pred)
wyn.svm.poly=ocena(wyn,"SVM wielomianowy dla wszystkich cech dla lewej nogi")
print(wyn.svm.poly)


#lewa noga - okrojony zestaw cech
#knn
final_to_calc_l = final[,-c(1,2, 12:17, 19)]
final_to_calc_l[8] = ifelse(final_to_calc_l$Sex == "M", 1, 0)
final_to_calc_l=final_to_calc_l[complete.cases(final_to_calc_l),]
split_l=sample.split(final_to_calc_l$Miesnielewejnogi,SplitRatio = 0.8)
train_set_l=subset(final_to_calc_l,split=TRUE)
test_set_l=subset(final_to_calc_l,split=FALSE)
trainX_l=train_set_l[,-10]
testX_l=test_set_l[,-10]
trainY_l=train_set_l[,10]
testY_l=test_set_l[,10]
trainX_s_l=scale(trainX_l)
testX_s_l=scale(testX_l)
knn.pred=knn(trainX_s_l, testX_s_l, final_to_calc_l$Miesnielewejnogi, k=5)
wyn=table(knn.pred,testY_l)
wyn.knn=ocena(wyn,"KNN dla wybranych cech dla lewej nogi")
print(wyn.knn)

#drzewo decyzyjne
classtree=rpart(formula=Miesnielewejnogi~.,data=train_set_l,method="class",control=rpart.control(maxdepth=3))#5,9
rpart.plot(classtree, box.palette = "RdBu",digit=-2)
pdd.pred=predict(classtree, test_set_l,type="class")
wyn=table(test_set_l$Miesnielewejnogi,pdd.pred)
wyn.pdd=ocena(wyn,"Drzewo dla wybranych cech dla lewej nogi")
wyn.pdd

#svm o j¹drze liniowym
svmfit=svm(Miesnielewejnogi~.,data=train_set_l, kernel="linear",
           scale=TRUE)
svm_lin_pred=predict(svmfit,test_set_l)
wyn=table(test_set_l$Miesnielewejnogi,svm_lin_pred)
wyn.svm.lin=ocena(wyn,"SVM liniowy dla wybranych cech dla lewej nogi")
print(wyn.svm.lin)

#svm o j¹drze wielomianowym
svmfitP=svm(Miesnielewejnogi~.,data=train_set_l,kernel="polynomial",
            degree=2)
svm.poly.pred=predict(svmfitP,test_set_l)
wyn=table(test_set_l$Miesnielewejnogi,svm.poly.pred)
wyn.svm.poly=ocena(wyn,"SVM wielomianowy dla wybranych cech dla lewej nogi")
print(wyn.svm.poly)

