setwd("C:/Users/csant/Desktop")
library(readr)
library(dplyr)
library(lubridate)
library(readxl)
library(openxlsx)
library(cluster)
library(FactoMineR)
library(factoextra)
library(gmodels)

trg <- read_csv("customer_x_default.csv")

#Voglio solo le osservazioni di dicembre alla fine di ciascun anno
trg1 <- trg %>% filter(month(DATE) == 12)
table(trg1$IN_DEFAULT_FLG)

#Voglio solo le imprese che non sono andate in default
trg2 <- trg1 %>% filter(DEFAULT_FLG != "Y")
table(trg2$IN_DEFAULT_FLG)

#IMPORT DATASET
#DATASET AGREEMENT
agr <- read_csv("customer_x_agreement.csv")
agr_inclusion<-read_excel("MetaData_Unina.xlsx", sheet= "I_agreement")
#View(agr_inclusion)
agr_inclusion$inclusion<-factor(agr_inclusion$inclusion)
agr_inclusion<-subset(agr_inclusion, inclusion=="Y")
#View(agr_inclusion)
agr_col<-agr_inclusion$variabile
agr<-agr[,agr_col]
#View(agr)

colnames(agr)[colnames(agr) == "DATA_DT"] <- "DATE"

#Discretizzazione
agr$Dpd3_D <- cut(agr$BAG_DPD3_CNT, breaks = c(0 ,1, 293), right = F, include.lowest = T)
table(agr$Dpd3_D)


#DATASET FINANCIAL
fin <- read_csv("customer_x_financial.csv")
fin_inclusion<-read_excel("MetaData_Unina.xlsx", sheet= "I_financial")
#View(fin_inclusion)
fin_inclusion$inclusion<-factor(fin_inclusion$inclusion)
fin_inclusion<-subset(fin_inclusion, inclusion=="Y")
#View(fin_inclusion)
fin_col<-fin_inclusion$variable
fin<-fin[,fin_col]
#View(fin)

colnames(fin)[colnames(fin) == "DATA_DT"] <- "DATE"


#DATASET SECTOR
sec <- read_csv("customer_x_sector.csv")


#DATASET TRANSACTION
trn <- read_csv("customer_x_transaction.csv")
#selezione delle variabili
trn_inclusion<-read_excel("transaction I.xlsx")
#View(trn_inclusion)
trn_inclusion$inclusion<-factor(trn_inclusion$inclusion)
trn_inclusion<-subset(trn_inclusion, inclusion== "Y")
#View(trn_inclusion)
trn_col<-trn_inclusion$variabile
trn<-trn[,trn_col]
colnames(trn)[colnames(trn) == "DATA_DT"] <- "DATE"
#View(trn)

#Voglio aggregare al 31-12
trn[is.na(trn)] <- 0 
sum_trn<-aggregate(cbind(BTN_IN_CNT, BTN_OUT_CNT, BTN_IN_AMT, BTN_OUT_AMT) ~ CUSTOMER_RK + year(DATE), data = trn,sum)
max_trn<-aggregate(cbind(BTN_IN_MAX_AMT, BTN_OUT_MAX_AMT) ~ CUSTOMER_RK + year(DATE), data = trn, max)
trn<-merge(sum_trn, max_trn, by = c("CUSTOMER_RK", "year(DATE)"))
trn$DATE <- as.Date(paste0(trn$`year(DATE)`, "-12-31"))
trn <- trn[, !(names(trn) %in% c("year(DATE)"))]


#UNIONE DATASET
df <- Reduce(function(x, y) merge(x, y, by = c("DATE", "CUSTOMER_RK")), list(trg2, agr, fin, sec, trn))
table(df$IN_DEFAULT_FLG)


#PREPROCESSING: PROBLEMA ANNO DI REPORTING
#table(df$DATE, df$BFD_CYN_BS_PERIOD_END_DT) 
df<-df[-which((year(df$BFD_CYN_BS_PERIOD_END_DT)+1)!=year(df$DATE)),]
df<-df[-which((year(df$BFD_CYN_CFS_PERIOD_END_DT)+1)!=year(df$DATE)),]
df<-df[-which((year(df$BFD_CYN_ISN_PERIOD_END_DT)+1)!=year(df$DATE)),]
#magari fino a 2 anni
#se sono NA?

#PREPROCESSING: VALORI CHE NON HANNO SENSO
sum(df$BTN_IN_CNT == 0 & df$BTN_IN_AMT != 0) #0
sum(df$BTN_OUT_CNT == 0 & df$BTN_OUT_AMT != 0) #22

df <- df[-which(df$BTN_OUT_CNT == 0 & df$BTN_OUT_AMT != 0), ]

#PREPROCESSING : NA
totna_comp <- rowSums(is.na(df))
df <- subset(df, totna_comp <= 7) #nel 95% delle variabili non hanno NA
df <- df %>%
  group_by(SECTOR_CD, IN_DEFAULT_FLG) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

summary(df)
table(df$IN_DEFAULT_FLG)#544 y, perse 150, considerare il rapporto?

#...


#CREAZIONE DATASET SENZA DATE E AGGIUNTA DI UNO SHIFT DI 2
df[,c("BFD_CYN_BS_PERIOD_END_DT", "BFD_PYN_BS_PERIOD_END_DT", "BFD_CYN_CFS_PERIOD_END_DT", "BFD_PYN_CFS_PERIOD_END_DT", "BFD_CYN_ISN_PERIOD_END_DT", "BFD_PYN_ISN_PERIOD_END_DT")] <-NULL
sum(is.na(df))
df <- na.omit(df)

df <- df %>%
  mutate(across(where(is.numeric), ~. + 2))


#FEATURE ENGINEERING: INDICI DI BILANCIO
#LIQUIDITA'
#Indice di disponibilità
short_ass <- colnames(df[,23:31])
ind_disp <- rowSums(df[,short_ass]) / df$BFD_CYN_LB_LP_STM ###
#Turnover
rvn <- (df$BFD_CYN_ISN_RVN + df$BFD_CYN_ISN_OPEXP_PROLL)
ind_turn <-rvn / df$BFD_CYN_AS ###
#Rotazione delle rimanenze di magazzino
inventory <- colnames(df[,23:24])
ind_rot_mag <- rvn / rowSums(df[,inventory]) #oss con Inf perchè hanno 0
#Sostenibilità degli oneri finanziari
ind_sost_on_fin <- df$BFD_CYN_ISN_FEXP_INT / rvn ###
#Debiti a breve termine su fatturato
ind_deb_su_fat <- df$BFD_CYN_LB_LP_STM / rvn ###
#Elasticità dell'attivo
ind_ela_att <- rowSums(df[,short_ass]) / df$BFD_CYN_AS ###
#Liquidità sul fatturato
ind_liq_fat <- df$BFD_CYN_AS_CRT_SINV_FA_CPA / rvn
#Acid Test
ind_acid <- (rowSums(df[,short_ass]) - rowSums(df[,inventory])) / df$BFD_CYN_LB_LP_STM
#MOL
mol <- df$BFD_CYN_ISN_RVN
#MOL su oneri finanziari e totale dei debiti
#ind_mol_on_deb <- mol/(df$BFD_CYN_ISN_FEXP_INT + ) #non ho il totale dei debiti
#Copertura oneri finanziari
ind_cop_on_fin <- mol / df$BFD_CYN_ISN_FEXP_INT
#Oneri finanziari su margine operativo lordo
ind_on_fin_mol <- df$BFD_CYN_ISN_FEXP_INT / mol

#REDDITIVITA'
#capitale proprio
cap_prop <- df$BFD_CYN_LB_EQT - df$BFD_CYN_LB_EQT_PYPRFT - df$BFD_CYN_LB_EQT_NTPRFT - df$BFD_CYN_LB_EQT_WOFF
#ROE
ind_roe <- df$BFD_CYN_ISN_NETPRFT/df$BFD_CYN_LB_EQT
#ROI
ind_roi <- df$BFD_CYN_ISN_RVN / df$BFD_CYN_AS
#ROS
ind_ros <- df$BFD_CYN_ISN_RVN / rvn
#ROD
ind_rod <- df$BFD_CYN_ISN_FEXP_INT / df$BFD_CYN_LB_LP_STM_OTH_OTH


#INDICI DI STRUTTURA FINANZIARIA
#
ind_aut_fin <- cap_prop / df$BFD_CYN_LB

#VARIAZIONI
#
ind_var_eq <- df$BFD_CYN_LB_EQT - df$BFD_PYN_LB_EQT
#
ind_var_net_prf <- df$BFD_CYN_LB_EQT_NTPRFT - df$BFD_PYN_LB_EQT_NTPRFT
#
ind_var_grs_prf <- df$BFD_CYN_ISN_GROSSPRFT - df$BFD_PYN_ISN_GROSSPRFT


#FEATURE ENGINEERING: INDICI SULLE TRANSAZIONI
#Media ammontare entrate
ind_med_in <- df$BTN_IN_AMT / df$BTN_IN_CNT
#Media ammontare uscite
ind_med_out <- df$BTN_OUT_AMT / df$BTN_OUT_CNT
#Ratio entrate su uscite
ind_in_out_ratio <- df$BTN_IN_AMT / df$BTN_OUT_AMT
#Ratio entrate massime su uscite massime
ind_max_in_out_ratio <- df$BTN_IN_MAX_AMT / df$BTN_OUT_MAX_AMT


#CREAZIONE DATASET DEFINITIVO
attach(df)
df_def <- data.frame(CUSTOMER_RK, IN_DEFAULT_FLG, DATE, ind_disp, ind_turn, ind_rot_mag, ind_sost_on_fin, ind_deb_su_fat,
                     ind_ela_att, ind_liq_fat, ind_acid, mol, ind_cop_on_fin, ind_on_fin_mol, ind_roe,
                     ind_roi, ind_ros, ind_rod, ind_aut_fin, BTN_IN_CNT, BTN_OUT_CNT, BTN_IN_AMT, BTN_OUT_AMT,
                     BTN_IN_MAX_AMT, BTN_OUT_MAX_AMT, ind_var_eq, ind_var_net_prf, ind_var_grs_prf,
                     ind_in_out_ratio, ind_med_in, ind_med_out, ind_max_in_out_ratio, BAG_NGRANT_UOVD_CNT,
                     BAG_DBAL_SUM, Dpd3_D, BAG_POVD500_DPD_DMAX, SECTOR_CD)

df_def <- df_def %>%
  mutate(across(where(is.numeric), ~round(., 2)))
summary(df_def)
length(which(is.na(df_def)))

#EXPLORATORY DATA ANALYSIS: VARIABILI SIGNIFICATIVE
#Variabili numeriche
insignificant_variables <- c()
for (i in names(df_def)[1:dim(df_def)[2]]) {
  if (is.numeric(df_def[[i]])) {
    wilcox_result <- wilcox.test( df_def[[i]]~df_def$IN_DEFAULT_FLG )
    if (wilcox_result$p.value > 0.06) {
      insignificant_variables <- c(insignificant_variables, i)
    }
  }
}    
print(insignificant_variables)
#elimino le variabili non significative
df_def <- df_def[, -which(names(df_def) %in% insignificant_variables)]


#Variabili categoriche
CrossTable(df_def$Dpd3_D , df_def$IN_DEFAULT_FLG, chisq = T, format = "SPSS")
#e per i settori?


#EXPLORATORY DATA ANALYSIS: I SETTORI SONO DISCRIMINANTI?
#Cluster Analysis
#Creiamo un dataset eliminando le variabili carattere (K-MEANS)
set.seed(12)
df_cls<-df_def[, -c(1,3)]
df_cls$SECTOR_CD<-factor(df_def$SECTOR_CD)
df_cls$IN_DEFAULT_FLG<-NULL
df_cls$Dpd3_D<-NULL
summary(df_cls)
#Creare un dataset con i settori che sono osservazioni date dai baricentri delle aziende appartenenti a ciascun settore
df_Sector <- df_cls %>%
  group_by(SECTOR_CD) %>%
  summarise_all(mean)

df_Sector<-as.data.frame(df_Sector)
rownames(df_Sector)<-df_Sector$SECTOR_CD
df_Sector$SECTOR_CD<-NULL

df_SS <- scale(df_Sector)

fviz_nbclust(df_SS, FUNcluster = kmeans, method = "wss") #3 e 5
km_clus <- kmeans(x = df_SS,centers = 4,iter.max = 100)
cls <- km_clus$cluster
table(km_clus$cluster) 
prop.table(table(km_clus$cluster))*100
km_clus$centers #vediamo quali variabili influenzano i gruppi e in che modo (il segno)
fviz_cluster(km_clus,data = df_SS,ggtheme = theme_classic())
df_def$Sector_CL<-cls[match(df_def$SECTOR_CD, names(cls))]

table(df_def$IN_DEFAULT_FLG, df_def$Sector_CL)
fisher.test(table(df_def$IN_DEFAULT_FLG, df_def$Sector_CL))

df_def<-df_def[-which(df_def$SECTOR_CD== "39S"| df_def$SECTOR_CD=="16S"),]
CrossTable(df_def$Sector_CL, df_def$IN_DEFAULT_FLG, chisq = T, format = "SPSS")

#LA PARTE MODELLISTICA CONTINUA SU PYTHON....
#RUN IL CODICE SUCCESSIVO PER LA VISIONE SU GITHUB
browseURL('https://github.com/carminesant1/HackING/blob/main/ING.ipynb ')

#percorso_file_excel <- "C:/Users/csant/Desktop/df_definitivo.xlsx"
#write.xlsx(df_def, percorso_file_excel, row.names = FALSE)

table(df_def$IN_DEFAULT_FLG)
