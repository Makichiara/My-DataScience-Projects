#IMPORT libraries
library(tidyverse)
library(foreign)
library(randomForest)
library(gbm)
library(tree)
library(caret)
library(dplyr)
library(purrr)
library(pROC) 
library(PRROC)
library(readxl)
library(readr)

#IMPORT DATA
data0<- read_excel("C:/Users/User/Desktop/Master R/mACHINE LEARNING RESCE/MATERIALE PROGETTO/1a-Popolazione-valori-assoluti (1).xlsx",sheet = "Tav. 1.1 Comuni")
data1<-read_delim("C:/Users/User/Desktop/Master R/mACHINE LEARNING RESCE/MATERIALE PROGETTO/Indicatori_2011_8milaC_Convert.csv",delim = ";")

#Data Preprocessing
a<-data0 %>% select(6,31) #selezione colonne da 6 a 31
a<-a[-(1:4),] #Eliminazione prime 4 righe
colnames(a)[1] <- "Denominazione.del.territorio" #Rinomina prima colonna
Data<-right_join(a, data1, by = "Denominazione.del.territorio") #unione dei dataset importati attraverso una right-join usando come chiave la colonna "Denominazione.del.territorio"

data<-as.data.frame(Data) #creazione Dataframe
data$V3<-NULL #rimozione treza colonna v3 costituita da valori nulli
data=na.omit(data) #rimozione valori nulli
nrow(data) #count valori nulli

Abs<-as.numeric(data$...31) - data$P1 # differenza tra due colonne del df: "...31" convertita in formato numerico (utilizzando as.numeric()) e della colonna "P1". Rappresenta la differenza della popolazione tra i valori del 2021 e del 2011 per ogni riga del df.
#questo ciclo verifica se c è uno spopolamento o meno se la popolazione è <= 0 o >0, rispettivamente. Se c è stato spopolamento, assegna il valore di Abs a =, assegna valore 1 altrimenti.
data$P1
for(i in 1:length(Abs)) {
  if (Abs[i]<=0){               # abs= 0 se c'è lo spopolamento
    Abs[i]=0
  } 
  else Abs[i]=1                #  abs=1 se lo spopolamento non c'è stato
}
Abs
data_all<-data_frame(data,Abs)
DATA<-data_all[,10:106]
DATA$Abs <- as.factor(DATA$Abs) #fattorizzazione variabili
levels(DATA$Abs) <- c("YES", "NO") # Yes=spopolamento. no= non spopolamento
DATa

#settiamo il seed per la ripetibilità dei dati
set.seed(11)
#creazione dataset di train (70% del dataset totale) in modo che la proporzione di spopolamento e non spopolamento sia mantenuta nei set di train e di test
trainIndex_0 <- createDataPartition(DATA$Abs, p = .7,list = FALSE,times = 1)   

imbal_train<- DATA[ trainIndex_0,] #train dataset
imbal_test<- DATA[ -trainIndex_0,] #test dataset
imbal_test


table(DATA$Abs) #conta quante volte ogni valore unico di compare nella colonna.
table(imbal_train$Abs) #conteggio sul train
table(imbal_test$Abs) #conteggio sul test


# Bilanciamento Pesi del dataset.
# I pesi vengono calcolati in base al conteggio effettivo di ciascuna classe nel set di addestramento assegnando un peso maggiore alle classi meno rappresentate.
# il conteggio di "YES" è inferiore al conteggio di "NO" nel set di addestramento, quindi la classe "YES" sarà più penalizzata, poiché verrà assegnato un peso più alto per bilanciare il suo conteggio inferiore durante l'addestramento del modello.
model_weights_1 <- ifelse(imbal_train$Abs == "NO",
                          (1/table(imbal_train$Abs)[1]) * 0.5,
                          (1/table(imbal_train$Abs)[2]) * 0.5)

# Funzione di controllo per il training
ctrl <- trainControl(method = "repeatedcv", #repeated cross validation
                     number = 10, #numero di fold
                     repeats = 5, #numero di ripetizioni per repeated cv
                     summaryFunction = twoClassSummary, #calcolo statistiche di riepilogo
                     classProbs = TRUE) #include valori di probabilità per le classi

###################        Modelling       #####################
#Applichiamo diversi modelli al dataset di training

#  Gradient boosting machine
orig_fit_gbm<- train(Abs ~ .,
                 data = imbal_train,
                 method = "gbm",
                 verbose = TRUE,
                 metric = "ROC", #metrica utilizzata ROC
                 trControl = ctrl)

# Neural Network
ctrl$seeds <- NA #imposta seed casuali diversi per ogni ripetizione del processo di addestramento, garantendo la casualizzazione adeguata durante la fase di addestramento del modello.
orig_fit_nne<- train(Abs ~ .,
                     data = imbal_train,
                     method = "nnet", #modello NN
                     verbose = TRUE,
                     metric = "ROC",
                     trControl = ctrl)

# LASSO
ctrl$seeds <- orig_fit_gbm$control$seeds #controllo dell'addestramento del modello orig_fit_lasso pari ai semi casuali utilizzati durante l'addestramento del modello orig_fit_gbm
orig_fit_lasso<- train(Abs ~ .,
                       data = imbal_train,
                       method = "glmnet", #modello elastic net
                       verbose = TRUE,
                       metric = "ROC",
                       trControl = ctrl)

#RANDOM fOREST
ctrl$seeds <- orig_fit_gbm$control$seeds
orig_fit_rf<- train(Abs ~ .,
                    data = imbal_train,
                    method = "rf", #modello random forest
                    verbose = TRUE,
                    metric = "ROC",
                    trControl = ctrl)


###########################     WEIGHTED MODELS    ####################
# Si utilizza il modello con i pesi da assegnare a ciascun campione durante il training. Utile quando ci sono classi sbilanciate nel dataset.
# Gradient Boosting Machine
ctrl$seeds <- orig_fit_gbm$control$seeds
weighted_fit <- train(Abs ~ .,
                      data = imbal_train,
                      method = "gbm",
                      verbose = TRUE,
                      weights = model_weights_1,
                      metric = "ROC",
                      trControl = ctrl)
# Random Forest
ctrl$seeds <- orig_fit_gbm$control$seeds
weighted_fit_rf <- train(Abs ~ .,
                         data = imbal_train,
                         method = "rf",
                         verbose = TRUE,
                         weights = model_weights_1,
                         metric = "ROC",
                         trControl = ctrl)

# LASSO  
ctrl$seeds <- orig_fit_gbm$control$seeds
weighted_fit_lasso <- train(Abs ~ .,
                            data = imbal_train,
                            method = "glmnet",
                            verbose = TRUE,
                            weights = model_weights_1,
                            metric = "ROC",
                            trControl = ctrl)

# NN
ctrl$seeds <- NA
weighted_fit_nne <- train(Abs ~ .,
                          data = imbal_train,
                          method = "nnet",
                          verbose = TRUE,
                          weights = model_weights_1,
                          metric = "ROC",
                          trControl = ctrl)



################       ROSE WEIGHTED MODELS    #######################
#campionamento delle classi effettuato utilizzando l'approccio di sovracampionamento ROSE (Random Over-Sampling Examples) 
#Si risolve il problema delle classi sbilanciate duplicando casualmente gli esempi della classe minoritaria, portando così a una distribuzione più equilibrata tra le classi nel set di dati di addestramento.

# GBM
ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "rose" 
rose_fit <- train(Abs ~ .,
                  data = imbal_train,
                  method = "gbm",
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = ctrl)

###  NN
ctrl$seeds <- NA
ctrl$sampling <- "rose" 
rose_fit_nne <- train(Abs ~ .,
                      data = imbal_train,
                      method = "nnet",
                      verbose = TRUE,
                      metric = "ROC",
                      trControl = ctrl)

# ELASTIC NET
ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "rose" 
rose_fit_lasso <- train(Abs ~ .,
                        data = imbal_train,
                        method = "glmnet",
                        verbose = TRUE,
                        metric = "ROC",
                        trControl = ctrl)

# RF
ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "rose" 
rose_fit_rf <- train(Abs ~ .,
                     data = imbal_train,
                     method = "rf",
                     verbose = TRUE,
                     metric = "ROC",
                     trControl = ctrl)


############################    UP    ######################
#utilizzo approccio di sovracampionamento "up" durante l'addestramento del modello. 
#Questo approccio consiste nel bilanciare le classi aumentando il numero di campioni della classe minoritaria fino a raggiungere una proporzione desiderata tra le classi. 
#Verranno generati più campioni per la classe minoritaria per equiparare il numero di campioni della classe maggioritaria.

# GBM
ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "up"
up_fit <- train(Abs ~ .,
                data = imbal_train,
                method = "gbm",
                verbose = TRUE,
                metric = "ROC",
                trControl = ctrl)

# NN 
ctrl$seeds <- NA
ctrl$sampling <- "up"
up_fit_nne <- train(Abs ~ .,
                    data = imbal_train,
                    method = "nnet",
                    verbose = TRUE,
                    metric = "ROC",
                    trControl = ctrl)

# LASSO

ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "up"
up_fit_lasso <- train(Abs ~ .,
                      data = imbal_train,
                      method = "glmnet",
                      verbose = TRUE,
                      metric = "ROC",
                      trControl = ctrl)


# RF
ctrl$seeds <- orig_fit_gbm$control$seeds
ctrl$sampling <- "up"
up_fit_rf <- train(Abs ~ .,
                   data = imbal_train,
                   method = "rf",
                   verbose = TRUE,
                   metric = "ROC",
                   trControl = ctrl)


#LISTA DI MODELLI origin
model_list_origin <- list(Original_GBM = orig_fit_gbm, 
                          Original_LASSO = orig_fit_lasso,
                          Original_NNE= orig_fit_nne,
                          Original_RF = orig_fit_rf)
#LISTA DI MODELLI  weighted
model_list_weighted <- list(weighted_GBM = weighted_fit, 
                            weighted_LASSO = weighted_fit_lasso,
                            weighted_NNE= weighted_fit_nne,
                            weighted_RF = weighted_fit_rf)

#LISTA DI MODELLI up
model_list_up <- list(up_GBM =up_fit,   
                      up_LASSO = up_fit_lasso,
                      up_NNE= up_fit_nne,
                      up_RF =up_fit_rf)

#LISTA DI MODELLI rose
model_list_rose <- list(rose_GBM =rose_fit,  
                        rose_LASSO = rose_fit_lasso,
                        rose_NNE= rose_fit_nne,
                        rose_RF = rose_fit_rf)



###############     Metrica: ROC    ###############
#FUNZIONE PER CALCOLO AREA UNDER ROC
test_roc <- function(model, data) {
  
  roc(data$Abs,
      predict(model, data, type = "prob")[, "YES"])
  
}

###########################    ROC CURVE  origin models           ##################
model_list_roc_origin <- model_list_origin %>%
  map(test_roc, data = imbal_test)

model_list_roc_origin %>%
  map(auc)

# GBM-----> Area under the curve: 0.882
# LASSO-----> Area under the curve: 0.8753
# NNE-------> Area under the curve: 0.7403
# RF---------> Area under the curve: 0.8831

###########################    ROC CURVE   weighted models           ##################
model_list_roc_weighted <- model_list_weighted %>%
  map(test_roc, data = imbal_test)

model_list_roc_weighted %>%
  map(auc)

# GBM-----> Area under the curve: 0.8798
# LASSO-----> Area under the curve: 0.874
# NNE-------> Area under the curve: 0.84
# RF---------> Area under the curve:  0.8831

###########################    ROC CURVE  up models           ##################

model_list_roc_up <- model_list_up %>%
  map(test_roc, data = imbal_test)

model_list_roc_up %>%
  map(auc)

# GBM-----> Area under the curve: 0.8826
# LASSO-----> Area under the curve: 0.8756
# NNE-------> Area under the curve: 0.703
# RF---------> Area under the curve: 0.8836

###########################    ROC CURVE  rose models           ##################

model_list_roc_rose <- model_list_rose %>%
  map(test_roc, data = imbal_test)

model_list_roc_rose %>%
  map(auc)

# GBM-----> Area under the curve: 0.8574
# LASSO-----> Area under the curve: 0.8707
# NNE-------> Area under the curve: 0.6566
# RF---------> Area under the curve: 0.8556


#########################    Precision Recall Curve FUNCTION   ################

calc_auprc <- function(model, data){
  index_class2 <- data$Abs == "YES"
  index_class1 <- data$Abs == "NO"
  predictions <- predict(model, data, type = "prob")
  pr.curve(predictions$YES[index_class2], predictions$YES[index_class1], curve = TRUE)
  
}

#########################  Lista dei 4 modelli Random Forest    ###############
model_list_rf <- list(Original_rf = orig_fit_rf,
                      Weighted_rf= weighted_fit_rf,
                      ROSE_rf= rose_fit_rf,
                      UP_rf= up_fit_rf)

######################## Lista dei 4 modelli lasso   ###################### 
model_list_lasso <- list(Original_lasso = orig_fit_lasso,
                         Weighted_lasso= weighted_fit_lasso,
                         ROSE_lasso= rose_fit_lasso,
                         UP_lasso= up_fit_lasso)


#################### Lista dei 3 modelli Neural Network   ##################
model_list_nne <- list(Original_nne = orig_fit_nne,
                       Weighted_nne= weighted_fit_nne,
                       ROSE_nne= rose_fit_nne,
                       UP_nne= up_fit_nne)


####################### Lista dei 4 modelli GBM  #########################
model_list_gbm <- list(Original_gbm = orig_fit_gbm,
                       Weighted_gbm = weighted_fit,
                       ROSE_gbm = rose_fit,
                       UP_gbm = up_fit)



##################### PRECISION RECALL FOR GBM   ######################

model_list_gbm_pr<- model_list_gbm  %>%
  map(calc_auprc, data = imbal_test)

model_list_gbm_pr %>%
  map(function(the_mod) the_mod$auc.integral) 

#Original 0.9389971
#Weighted 0.9374868
#ROSE  0.928347
#UP 0.9420432

# sum(imbal_test$Abs == "YES")/nrow(imbal_test) è 0.69. Minore di ogni valore trovato, quindi 
# i modelli funzionano tutti bene nella predizione

##################### PRECISION RECALL FOR LASSO   ######################

model_list_lasso_pr<- model_list_lasso%>%
  map(calc_auprc, data = imbal_test)

model_list_lasso_pr %>%
  map(function(the_mod) the_mod$auc.integral)

# Original_lasso 0.934933
# Weighted_lasso 0.9336658
# ROSE_lasso 0.9341757
# UP_lasso  0.935821

##################### PRECISION RECALL FOR RANDOM FOREST   ######################

model_list_rf_pr<- model_list_rf%>%
  map(calc_auprc, data = imbal_test)

model_list_rf_pr %>%
  map(function(the_mod) the_mod$auc.integral)
# Original_rf 0.9425241
# Weighted_rf  0.9425241
# ROSE_rf 0.9298373
# UP_rf 0.9426982

##################### PRECISION RECALL FOR NN  ######################

model_list_nne_pr<- model_list_nne%>%
  map(calc_auprc, data = imbal_test)

model_list_nne_pr %>%
  map(function(the_mod) the_mod$auc.integral)

# Original_nne  0.8481629
# Weighted_nne 0.9136813
# ROSE_nne 0.7776371
# UP_nne  0.8122858






#####################       PLOT DELLE PRC DEL RANDOM FOREST    ##############
results_list_pr <- list(NA)
num_mod <- 1
for(the_pr in model_list_rf_pr){
    results_list_pr[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                           precision = the_pr$curve[, 2],
                                           model = names(model_list_rf_pr)[num_mod])
    num_mod <- num_mod + 1}

results_df_pr <- bind_rows(results_list_pr)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Abs == "YES")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()


########################    PLOT ROC DI  DEI QUATTRO MODELLI ORIGIN ###########

results_list_roc_origin <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc_origin){
  results_list_roc_origin[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list_origin)[num_mod])
  num_mod <- num_mod + 1
  
}


results_df_roc_origin <- bind_rows(results_list_roc_origin)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
ggplot(aes(x = fpr,  y = tpr, group = model), data = results_df_roc_origin) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_classic()




#######################  PLOT  ROC DEL RANDOM FOREST    ##########

model_list_roc_RF <- model_list_rf %>%
  map(test_roc, data = imbal_test)

model_list_roc_RF %>%
  map(auc)

results_list_roc_RF <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc_RF){
  results_list_roc_RF[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list_rf)[num_mod])
  num_mod <- num_mod + 1
  
}
results_df_roc_RF <- bind_rows( results_list_roc_RF)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p1<-ggplot(aes(x = fpr,  y = tpr, group = model), data =  results_df_roc_RF ) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_classic()
p1+ggtitle("ROC dei quattro modelli Random Forest")


#######################  PLOT  ROC GBM  ##########

model_list_roc_GBM <- model_list_gbm %>%
  map(test_roc, data = imbal_test)

model_list_roc_GBM %>%
  map(auc)

results_list_roc_GBM <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc_GBM){
  results_list_roc_GBM[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list_gbm)[num_mod])
  num_mod <- num_mod + 1
  
}
results_df_roc_GBM <- bind_rows( results_list_roc_GBM)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p2<-ggplot(aes(x = fpr,  y = tpr, group = model), data =  results_df_roc_GBM ) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_classic()
p2+ggtitle("ROC dei quattro modelli Gradient Boosting Machine")


#######################  PLOT  ROC DEL ELASTIC    ##########

model_list_roc_LASSO <- model_list_lasso %>%
  map(test_roc, data = imbal_test)

model_list_roc_LASSO %>%
  map(auc)

results_list_roc_LASSO <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc_LASSO){
  results_list_roc_LASSO[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list_lasso)[num_mod])
  num_mod <- num_mod + 1
  
}
results_df_roc_LASSO <- bind_rows( results_list_roc_LASSO)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p3<-ggplot(aes(x = fpr,  y = tpr, group = model), data =  results_df_roc_LASSO ) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_classic()
p3+ggtitle("ROC dei quattro modelli Elastic Net")


#######################  PLOT  ROC DEL NN    ##########

model_list_roc_NN <- model_list_nne %>%
  map(test_roc, data = imbal_test)

model_list_roc_NN %>%
  map(auc)

results_list_roc_NN <- list(NA)
num_mod <- 1
for(the_roc in model_list_roc_NN){
  results_list_roc_NN[[num_mod]] <- 
    data_frame(tpr = the_roc$sensitivities,
               fpr = 1 - the_roc$specificities,
               model = names(model_list_nne)[num_mod])
  num_mod <- num_mod + 1
  
}
results_df_roc_NN <- bind_rows( results_list_roc_NN)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p4<-ggplot(aes(x = fpr,  y = tpr, group = model), data =  results_df_roc_NN ) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col)+
  geom_abline(intercept = 0, slope = 1, color = "gray", size = 1) +
  theme_classic()

p4+ggtitle("ROC dei quattro modelli Neural Network")



#####################     Performance dei modelli   ###############
# original RF
p_RF_original<- predict(orig_fit_rf,imbal_test)
confusionMatrix(p_RF_original,imbal_test$Abs)

# weighted RF
p_RF_weighted<- predict(weighted_fit_rf,imbal_test)
confusionMatrix(p_RF_weighted,imbal_test$Abs)

# up RF
p_RF_up<- predict(up_fit_rf,imbal_test)
confusionMatrix(p_RF_up,imbal_test$Abs)


# rose RF
p_RF_rose<- predict(rose_fit_rf,imbal_test)
confusionMatrix(p_RF_rose,imbal_test$Abs)


# original GBM
p_GBM_original<- predict(orig_fit_gbm,imbal_test)
confusionMatrix(p_GBM_original,imbal_test$Abs)

# weighted GBM
p_GBM_weighted<- predict(weighted_fit,imbal_test)
confusionMatrix(p_GBM_weighted,imbal_test$Abs)

# up GBM
p_GBM_up<- predict(up_fit,imbal_test)
confusionMatrix(p_GBM_up,imbal_test$Abs)


# rose GBM
p_GBM_rose<- predict(rose_fit,imbal_test)
confusionMatrix(p_GBM_rose,imbal_test$Abs)



# original LASSO
p_NET_original<- predict(orig_fit_lasso,imbal_test)
confusionMatrix(p_NET_original,imbal_test$Abs)

# weighted LASSO
p_NET_weighted<- predict(weighted_fit_lasso,imbal_test)
confusionMatrix(p_NET_weighted,imbal_test$Abs)

# up LASSO
p_NET_up<- predict(up_fit_lasso,imbal_test)
confusionMatrix(p_NET_up,imbal_test$Abs)


# rose LASSO
p_NET_rose<- predict(rose_fit_lasso,imbal_test)
confusionMatrix(p_NET_rose,imbal_test$Abs)


# original NEURAL
p_NN_original<- predict(orig_fit_nne,imbal_test)
confusionMatrix(p_NN_original,imbal_test$Abs)

# weighted NEURAL
p_NN_weighted<- predict(weighted_fit_nne,imbal_test)
confusionMatrix(p_NN_weighted,imbal_test$Abs)

# up NEURAL
p_NN_up<- predict(up_fit_nne,imbal_test)
confusionMatrix(p_NN_up,imbal_test$Abs)


# rose NEURAL
p_NN_rose<- predict(rose_fit_nne,imbal_test)
confusionMatrix(p_NN_rose,imbal_test$Abs)



##################         SCELGO IL RANDOM FOREST    ################

################      Delong's Test tra i modelli  ##################


# Delong test tra RF_original e tutto il resto
roc.test(model_list_roc_origin$Original_RF, model_list_roc_weighted$weighted_RF) # p-value=1, Original RF non batte RF wheighted
roc.test(model_list_roc_origin$Original_RF, model_list_roc_rose$rose_RF) # p-value = 0.89. Original RF non batte RF up
roc.test(model_list_roc_origin$Original_RF, model_list_roc_up$up_RF) # p-value = 2.063e-06. Original RF  batte RF rose

roc.test(model_list_roc_origin$Original_RF, model_list_roc_origin$Original_GBM) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_weighted$weighted_GBM) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_rose$rose_GBM) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_up$up_GBM) # p-value = 2.063e-06. Original RF  batte RF rose

roc.test(model_list_roc_origin$Original_RF, model_list_roc_origin$Original_LASSO) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_weighted$weighted_LASSO) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_rose$rose_LASSO) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_up$up_LASSO) # p-value = 2.063e-06. Original RF  batte RF rose

roc.test(model_list_roc_origin$Original_RF, model_list_roc_origin$Original_NNE) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_weighted$weighted_NNE) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_rose$rose_NNE) # p-value = 2.063e-06. Original RF  batte RF rose
roc.test(model_list_roc_origin$Original_RF, model_list_roc_up$up_NNE) # p-value = 2.063e-06. Original RF  batte RF rose



#######################     PLOT PRECISION RECALL RF    #############


results_list_pr_rf <- list(NA)
num_mod <- 1
for(the_pr in model_list_rf_pr){
  results_list_pr_rf[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                           precision = the_pr$curve[, 2],
                                           model = names(model_list_rf_pr)[num_mod])
  num_mod <- num_mod + 1
}
results_df_pr_rf <- bind_rows(results_list_pr_rf)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p5<-ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr_rf) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Abs == "YES")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()
p5+ggtitle("PRC dei quattro modelli Random Forest")


#######################     PLOT PRECISION RECALL GBM   #############


results_list_pr_gbm <- list(NA)
num_mod <- 1
for(the_pr in model_list_gbm_pr){
  results_list_pr_gbm[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                              precision = the_pr$curve[, 2],
                                              model = names(model_list_gbm_pr)[num_mod])
  num_mod <- num_mod + 1
}
results_df_pr_gbm <- bind_rows(results_list_pr_gbm)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p6<-ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr_gbm) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Abs == "YES")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()
p6+ggtitle("PRC dei quattro modelli Gradient Boosting Machine")


#######################     PLOT PRECISION RECALL LASSO   #############


results_list_pr_las <- list(NA)
num_mod <- 1
for(the_pr in model_list_lasso_pr){
  results_list_pr_las[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                               precision = the_pr$curve[, 2],
                                               model = names(model_list_lasso_pr)[num_mod])
  num_mod <- num_mod + 1
}
results_df_pr_las <- bind_rows(results_list_pr_las)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p7<-ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr_las) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Abs == "YES")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()
p7+ggtitle("PRC dei quattro modelli Gradient Boosting Machine")



#######################     PLOT PRECISION RECALL NNE  #############


results_list_pr_nne <- list(NA)
num_mod <- 1
for(the_pr in model_list_nne_pr){
  results_list_pr_nne[[num_mod]] <- data_frame(recall = the_pr$curve[, 1],
                                               precision = the_pr$curve[, 2],
                                               model = names(model_list_nne_pr)[num_mod])
  num_mod <- num_mod + 1
}
results_df_pr_nne <- bind_rows(results_list_pr_nne)
custom_col <- c("#000000", "#009E73", "#0072B2", "#D55E00")
p8<-ggplot(aes(x = recall, y = precision, group = model), data = results_df_pr_nne) +
  geom_line(aes(color = model), size = 1) +
  scale_color_manual(values = custom_col) +
  geom_abline(intercept = sum(imbal_test$Abs == "YES")/nrow(imbal_test),
              slope = 0, color = "gray", size = 1) +
  theme_bw()
p8+ggtitle("PRC dei quattro modelli Neural Network")


#############       FEATURE IMPORTANCE RF MODEL   ###############


vip::vip(orig_fit_rf,num_features = 7L)


########################    LOGISTICA   ##########################


# LOGIT
ctrl$seeds <- orig_fit_gbm$control$seeds
orig_fit_logit<- train(Abs ~ .,
                       data = imbal_train,
                       method = "glmnet",
                       verbose = TRUE,
                       metric = "ROC",
                       trControl = ctrl)

#####################     Performance dei modelli   ###############
# original logit
p_log_original<- predict(orig_fit_logit,imbal_test)
confusionMatrix(p_log_original,imbal_test$Abs)


###################### Lista dei 4 modelli GBM  #########################
model_list_logit <- list(Original_logit= orig_fit_logit)

###########################    ROC CURVE  origin models  logit         ##################
model_list_roc_origin_logit<- model_list_logit %>%
  map(test_roc, data = imbal_test)

model_list_roc_origin_logit %>%
  map(auc)


##################### PRECISION RECALL FOR GBM   ######################

model_list_logit_pr<- model_list_logit  %>%
  map(calc_auprc, data = imbal_test)

model_list_logit_pr %>%
  map(function(the_mod) the_mod$auc.integral) 




#######################     PARTIAL PLOT    ################


### partial polts RF. Impatto parziale di una variabile sull'altra

M= as.numeric(orig_fit_rf$bestTune)
DATA_2<-data_all[,10:106]
DATA_2$Abs=as.factor(DATA_2$Abs) 
DATA_2$Abs = relevel(DATA_2$Abs, "1")

library(randomForest)
Model.rf <- randomForest(factor(Abs)~.,data=DATA_2,mtry=M)

importanza<-importance(Model.rf)

library(pdp)

partial(Model.rf, pred.var = c("P4","A11"), plot = TRUE, prob = TRUE, rug = TRUE)

partial(Model.rf, pred.var = c("P4","L3"), plot = TRUE, prob = TRUE, rug = TRUE)

partial(Model.rf, pred.var = c("P7","F4"), plot = TRUE, prob = TRUE, rug = TRUE)

partial(Model.rf, pred.var = c("L12","M1"), plot = TRUE, prob = TRUE, rug = TRUE)







