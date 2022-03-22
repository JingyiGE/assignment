# Zcode: Z0174422

install.packages('readr')
install.packages('skimr')
install.packages("tidyverse")
install.packages("GGally")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("data.table")
install.packages("mlr3verse")
install.packages("ranger")
install.packages("ggpubr")

library("tidyverse")
library("ggplot2")
library("GGally")
library("DataExplorer")
library("data.table")
library("mlr3verse")


#Heart failure data

#Variable information:
#age: age of patient
#anaemia: diagnosis of anaemie (a decrease in total red blood cells or hemoglobin in blood)
#creatinine_phosphokinase: blood concentration of enzyme CPK (mcg/L)
#diabetes: diagnosis of diabetes
#ejection_fraction: proportion of blood leaving the heart on each contraction (%)
#high_blood_pressure: diagnosis of hypertension
#platelets: blood concentration of platelets (kiloplatelets/mL)
#serum_creatinine: blood concentration of serum creatinine (mg/dL).  Normal levels approx 0.6 to 1.35.  Elevation may imply poor kidney function.
#serum_sodium: blood concentration of serum sodium (mEq/L).  Normal levels approx 135 to 145.  Hyponatremia risk at low levels.
#sex: male=1, female=0
#smoking: patient is a smoker
#time: follow-up period (days)
#fatal_mi: whether the patient suffered a fatal myocardial infarction during the follow-up period

Heart_data<-readr::read_csv("https://www.louisaslett.com/Courses/MISCADA/heart_failure.csv")
#View(Heart_data)
skimr::skim(Heart_data)
#DataExplorer::plot_histogram(Heart_data, ncol = 3)


#### Data preprocessing ###
#abnormal serum_creatinine 
Heart_data <- Heart_data %>%
  mutate(serum_creatinine_level = case_when(
    serum_creatinine > 1.35 ~ "high_serum_creatinine",
    TRUE ~ "normal"
  ))

#abnormal serum_sodium 
Heart_data <- Heart_data %>%
  mutate(serum_sodium_level = case_when(
    serum_sodium < 135 ~ "low_serum_sodium",
    TRUE ~ "normal"
  ))

#data categorical (for plot)
Heart_data <- Heart_data %>%
  mutate(suffer_fatal_mi = case_when(
    fatal_mi == 1 ~ "suffer_fatal_mi",
    TRUE ~ "none"
  ))
Heart_data <- Heart_data %>%
  mutate(sex_1 = case_when(
    sex == 0 ~ "female",
    TRUE ~ "male"
  ))
Heart_data <- Heart_data %>%
  mutate(diabetes_1 = case_when(
    diabetes == 0 ~ "none",
    TRUE ~ "diabetes"
  ))
Heart_data <- Heart_data %>%
  mutate(anaemia_1 = case_when(
    anaemia == 0 ~ "none",
    TRUE ~ "anaemia"
  ))
Heart_data <- Heart_data %>%
  mutate(ejection_fraction_1 = case_when(
    ejection_fraction < 50 ~ "low_ejection",
    ejection_fraction < 70 ~ "normal",
    TRUE ~ "high_ejection"
  ))
Heart_data <- Heart_data %>%
  mutate(high_blood_pressure_1 = case_when(
    high_blood_pressure == 0 ~ "none",
    TRUE ~ "high_blood_pressure"
  ))
Heart_data <- Heart_data %>%
  mutate(smoking_1 = case_when(
    smoking == 0 ~ "none",
    TRUE ~ "smoking"
  ))
Heart_data <- Heart_data %>%
  mutate(smoking_1 = case_when(
    smoking == 0 ~ "none",
    TRUE ~ "smoking"
  ))
Heart_data <- Heart_data %>%
  mutate(platelets_1 = case_when(
    platelets < 100000 ~ "low_platelets",
    ejection_fraction < 300000 ~ "normal",
    TRUE ~ "high_platelets"
  ))
Heart_data <- Heart_data %>%
  mutate(creatinine_phosphokinase = case_when(
    platelets < 10 ~ "low_creatinine",
    ejection_fraction < 120 ~ "normal",
    TRUE ~ "high_creatinine"
  ))

Heart_data <- Heart_data %>% 
  select(-serum_creatinine, -serum_sodium, -time)

# pair plot
# (Heart_data %>%
#          select(suffer_fatal_mi, anaemia, diabetes, ejection_fraction, high_blood_pressure, platelets, serum_creatinine_level, serum_sodium_level, sex, smoking),
#       aes(color = suffer_fatal_mi))

ggpairs(Heart_data %>%
          select(suffer_fatal_mi, ejection_fraction, platelets, serum_creatinine_level, serum_sodium_level),
        aes(color = suffer_fatal_mi))

# age group
Heart_data <- Heart_data %>%
  mutate(agegroup = case_when(
    age < 50 ~ "40s",
    age < 60 ~ "50s",
    age < 70 ~ "60s",
    age < 80 ~ "70s",
    age < 90 ~ "80s",
    age < 100 ~ "90s",
    TRUE ~ "none"
  ))

Heart_data.byagegroup <- Heart_data %>% 
  group_by(agegroup) %>% 
  summarise(total = n(),
            suffer_fatal_total = sum(fatal_mi),
            pct.suffer = suffer_fatal_total/total*100)

ggplot(Heart_data.byagegroup %>% arrange(desc(total)),
       aes(x = agegroup, y = pct.suffer)) +
  geom_col()

p1 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = agegroup), position = "fill")
p2 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = serum_sodium_level), position = "fill")
p3 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = serum_creatinine_level), position = "fill")
p4 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = sex_1), position = "fill")
p5 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = high_blood_pressure_1), position = "fill")
p6 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = smoking_1 ), position = "fill")
p7 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = ejection_fraction_1  ), position = "fill")
p8 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = anaemia_1 ), position = "fill")
p9 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = diabetes_1 ), position = "fill")
p10 <- ggplot(data = Heart_data) + 
  geom_bar(mapping = aes(x = suffer_fatal_mi, fill = platelets_1 ), position = "fill")

ggpubr::ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10)

DataExplorer::plot_boxplot(Heart_data, by = "suffer_fatal_mi", ncol = 5)

set.seed(1) 
#set.seed(11)
#set.seed(23)
#set.seed(3344) 
#set.seed(876) 


### Data preprocess end ###

Heart_data_1 <- readr::read_csv("https://www.louisaslett.com/Courses/MISCADA/heart_failure.csv")


Heart_data_1$fatal_mi <- as.factor(Heart_data_1$fatal_mi)
Heart_data_1 <- Heart_data_1 %>% 
  select(-time)
Heart_data_3 <- Heart_data_1
Heart_data_3 <- Heart_data_3 %>% 
  select(-creatinine_phosphokinase, -sex)

## task
Heart_data_3$fatal_mi <- as.factor(Heart_data_3$fatal_mi)
heart_task <- TaskClassif$new("fatalmyocardial", Heart_data_3 , target = "fatal_mi", positive = "1")

#cross-validation
cv5 <- rsmp("cv", folds = 5)
cv5$instantiate(heart_task)

lrn_baseline <- lrn("classif.featureless", predict_type = "prob")
lrn_cart <- lrn("classif.rpart", predict_type = "prob")
lrn_ranger   <- lrn("classif.ranger", predict_type = "prob")
lrn_log_reg  <- lrn("classif.log_reg", predict_type = "prob")

#### test
res <- benchmark(data.table(
  task       = list(heart_task),
  learner    = list(lrn_baseline,
                    lrn_cart,
                    lrn_ranger,
                    lrn_log_reg),
  resampling = list(cv5)
), store_models = TRUE)

res$aggregate(list(msr("classif.ce"),
                   msr("classif.acc"),
                   msr("classif.auc"),
                   msr("classif.fpr"),
                   msr("classif.fnr")))

trees <- res$resample_result(2)
tree1 <- trees$learners[[3]]
tree1_rpart <- tree1$model
plot(tree1_rpart, compress = TRUE, margin = 0.1)
text(tree1_rpart, use.n = TRUE, cex = 0.8)
plot(res$resample_result(2)$learners[[5]]$model, compress = TRUE, margin = 0.1)
text(res$resample_result(2)$learners[[5]]$model, use.n = TRUE, cex = 0.8)
lrn_cart_cv <- lrn("classif.rpart", predict_type = "prob", xval = 10)
res_cart_cv <- resample(heart_task, lrn_cart_cv, cv5, store_models = TRUE)
print(res_cart_cv$l)
rpart::plotcp(res_cart_cv$learners[[5]]$model)
res_cart_cv$learners[[5]]$model$variable.importance
#cp=tree1_rpart$cptable[which.min(dtree$cptable[,"xerror"])
print(tree1_rpart)
tree1_rpart$variable.importance

res_cart_cv$learners[[1]]$model$cptable
tree1_rpart$cptable

################################### improved model start ##############################
set.seed(1234)

#trees
lrn_cart_cp <- lrn("classif.rpart", predict_type = "prob", cp=0.0325)

#random forest
lrn_ranger_low_nt <- lrn("classif.ranger", predict_type = "prob", num.trees = 5, mtry = 1)
lrn_ranger_high_nt <- lrn("classif.ranger", predict_type = "prob", num.trees = 900, mtry = 9)

#log-reg
lrn_log_reg_it  <- lrn("classif.log_reg", predict_type = "prob", maxit=40)

# improved model performance (8 MODELS)
res <- benchmark(data.table(
  task       = list(heart_task),
  learner    = list(lrn_baseline,
                    lrn_cart,
                    lrn_cart_cp,
                    lrn_ranger,
                    lrn_ranger_low_nt,
                    lrn_ranger_high_nt,
                    lrn_log_reg,
                    lrn_log_reg_it),
  resampling = list(cv5)
), store_models = TRUE)

res$aggregate(list(msr("classif.ce"),
                   msr("classif.acc"),
                   msr("classif.auc"),
                   msr("classif.fpr"),
                   msr("classif.fnr")))
################################### improved model end ##############################

#### only for plot ####

res <- benchmark(data.table(
  task       = list(heart_task),
  learner    = list(lrn_baseline,
                    lrn_cart,
                    lrn_ranger_high_nt,
                    lrn_log_reg),
  resampling = list(cv5)
), store_models = TRUE)

res$aggregate(msr("classif.fnr"))

vy <- c('lrn_baseline','lrn_cart','lrn_ranger_high_nt','lrn_log_reg')
vx <- c(1.0000000,0.4615205,0.4064327,0.5763450)
vv <- data.frame(vx, vy)

p_1 <- ggplot(vv,aes(x=vx,y=vy,fill=vy))+geom_bar(position="dodge",stat="identity")
p_1

measures = msrs(c("classif.ce", "classif.acc", "classif.auc","classif.fnr"))
performances = res$aggregate(measures)
performances[, .(learner_id, classif.ce,classif.acc, classif.auc, classif.fnr)]
fnr <- performances$classif.fnr
fnr
acc <- performances$classif.acc
acc
auc <- performances$classif.auc
auc
#leaner_name <- c('lrn_baseline','lrn_cart','lrn_cart_cp','lrn_ranger','lrn_ranger_low_nt','lrn_ranger_high_nt','lrn_log_reg','lrn_log_reg_it')
#plot(res)
#print(res)
#res$s

x <- c('lrn_baseline','lrn_cart','lrn_cart_cp',
       'lrn_ranger','lrn_ranger_low_nt','lrn_ranger_high_nt',
       'lrn_log_reg','lrn_log_reg_it','lrn_baseline',
       'lrn_cart','lrn_cart_cp','lrn_ranger',
       'lrn_ranger_low_nt','lrn_ranger_high_nt','lrn_log_reg',
       'lrn_log_reg_it','lrn_baseline','lrn_cart',
       'lrn_cart_cp','lrn_ranger','lrn_ranger_low_nt',
       'lrn_ranger_high_nt','lrn_log_reg','lrn_log_reg_it')
y <- c(0.6789831, 0.7157062, 0.7423729, 0.7322034, 0.7123729, 0.7657062, 0.7421469, 0.7421469,
       0.5000000, 0.7379377, 0.7280463, 0.7957596, 0.7227694, 0.7792757, 0.7719343, 0.7719343,
       1.0000000, 0.4615205, 0.3960234, 0.5169591, 0.7451170, 0.3853801, 0.5763450, 0.5763450)
label <- c('acc', 'acc', 'acc',
           'acc', 'acc', 'acc',
           'acc', 'acc', 'auc',
           'auc', 'auc', 'auc', 
           'auc', 'auc','auc', 
           'auc',  'fnr', 'fnr', 
           'fnr',  'fnr', 'fnr', 
           'fnr',  'fnr', 'fnr')
plot_data <- data.frame(label, x, y)


variable<-c('serum_creatinine','ejection_fraction','age','serum_sodium','platelets','anaemia','smoking','high_blood_pressure')
importances <- c(17.9136483, 11.8201699, 4.3224115, 4.2370069,2.1068922, 0.1933270, 0.1933270, 0.0819161)
data_variable <- data.frame(variable,importances)

ggplot(data = data_variable,mapping = aes(x = variable, y = importances, fill=variable,group=factor(1)))+
  geom_bar(stat="identity")


p_1 <- ggplot(plot_data,aes(x=x,y=y,fill=label))+geom_bar(position="dodge",stat="identity")
p_1
#### only for plot end ####
