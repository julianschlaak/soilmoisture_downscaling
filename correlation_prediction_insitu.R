#This script extracts point values, at the coordinates of the measuring stations, from the raster data. 
#Afterwards the extracted data are coorelated with the values of the station (daily mean value and flight time) and output as a table.
#Plot part can be ignored

library(rgdal)
library(raster)
library(RStoolbox)
library(sp)
library(dplyr)
library(ggplot2)
library(ggpubr)
install.packages("xlsx")
library(xlsx)

rm(list = ls())
setwd("F:\\loew")
#load swi_predictions
swi_pred_stack <- stack(list.files(path = "SWI_Predictions", pattern =  "*.tif$", full.names = T))
#load insitu_data daily_mean and at 5 pm (Aerial survey)
mess_punkte <- readOGR("sm_shapes\\daily_means_32633.shp")
messpunkte_17 <- readOGR("sm_shapes\\17_Uhr_32633.shp")
#shp as df
df_messpunkte <- as.data.frame(mess_punkte)
df_messpunkte_17 <- as.data.frame(messpunkte_17)
#extract points
rasValue_1 = extract(swi_pred_stack, mess_punkte)
rasValue_17 = extract(swi_pred_stack, messpunkte_17)
#as df 
df_val <- as.data.frame(rasValue_1)
df_val_17 <- as.data.frame(rasValue_17)
#remove redundant data (every second colum per date)
only_swi<- df_val %>%  select(everything()[c(TRUE, FALSE)])
only_swi_17<- df_val_17 %>%  select(everything()[c(TRUE, FALSE)])
#combine name of station and extractet_data
df_val_n <- cbind(df_messpunkte[1], only_swi)
df_val_n_17 <- cbind(df_messpunkte_17[1], only_swi_17)
#merge insitu and predicted values by station
df_join <- merge(df_messpunkte,df_val_n, by = "Station")
df_join_17 <- merge(df_messpunkte_17,df_val_n_17, by = "Station")
#replace Null with numeric 0
df_join[df_join== "NULL"] <- 0.0
df_join_17[df_join_17=="NULL"] <- 0.0

##correl an change to numeric 
#create empty lists
correl_list = list()
correl_list_17 = list()
diff_day = list()
diff_day_mean = list()
diff_17_mean =list()
diff_17 = list()

for(i in 1:ncol(only_swi)){
  #get columns ans store in temp. variable (insitu).
  temp_insitu <- df_join[i+1]
  #rename to iterate
  names(temp_insitu) <-1
  #store as numeríc
  temp<- as.numeric(temp_insitu$`1`)
  #same with 5 pm values
  temp_insitu_17 <- df_join_17[i+1]
  names(temp_insitu_17) <-1
  temp_17 <- as.numeric(temp_insitu_17$`1`)
  #store the swi_prediction_values
  temp_swi <- df_join[28+i]
  temp_swi_17 <- df_join_17[28+i]
  #create difference
  diff_day <- temp - temp_swi
  #average
  mean_t <- mean(diff_day[,1])
  #store in list
  diff_day_mean[i] <- mean_t
  #same with 5 pm values
  diff_17 <- temp_17- temp_swi_17
  mean_17 <- mean(diff_17[,1])
  diff_17_mean[i] <- mean_17 
  #correlate swi_predict values with insitu values
  temp_cor<- cor(temp, temp_swi, use="complete.obs")
  temp_cor_17 <- cor(temp_17, temp_swi_17, use="complete.obs")
  correl_list[i] = temp_cor
  correl_list_17[i] = temp_cor_17
}
#get the dates
namen <-  names(df_join[2:24])
#combine all values per date 
out_tab <- as.data.frame(cbind(namen,correl_list,correl_list_17,diff_day_mean, diff_17_mean))
#rename columns
names(out_tab) <- c("Datum", "Korrelation Tagesmittel", "Korrelation 17 Uhr", "Differenz Mittel-Pred", "Differenz 17Uhr- Pred")
out_tab[,1]  <- substr(out_tab[,1],2,99)
#export table
write.xlsx(out_tab, file = "Ausgabe_Korrelation.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

#plot not n
#create insitu df
df_insitu <- df_join[,2:24]
#change datatype to numeric 
insitu_numeric <- sapply(df_insitu, as.numeric)
#average
mean_insitu <- apply(insitu_numeric , 1, mean,na.rm = TRUE )
#create predict df 
df_pred <- df_join[,29:51]
#average
mean_pred <- apply(df_pred, 1, mean,na.rm = TRUE )
#plot with r_standard 
plot(mean_insitu, mean_pred, main = "Streudiagram Bodenfeuchte", xlab = "Bodenfeuchte in % \n Insitumesseungen", 
     ylab = "Bodenfeuchte in % \n Modell", las = 1, pch= 16,cex =1,  col =2, xlim = c(0.1, 0.5), ylim =c(0.1,0.5))
abline(lm(mean_insitu~mean_pred))
#create combined df 
df_combi <- as.data.frame(cbind(mean_pred, mean_insitu))
# use ggplot to visualise
ggplot(df_combi, aes(x=df_combi$mean_insitu, y=df_combi$mean_pred))+
  geom_point( size = 2,shape="circle filled", fill="red",col = "black")+
  labs(x="Insitu \n Bodenfeuchte in %", y="Modell Bodenfeuchte in %")+
  ggtitle("Streudiagramm Bodenfeuchte 2019")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method = "lm", se = FALSE, col = "black", )+
  xlim(0.1, 0.5)+ ylim(0.2,0.3)+
  stat_regline_equation(label.y = 0.290, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 0.280, aes(label = ..rr.label..))
