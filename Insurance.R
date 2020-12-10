library(tidyverse)
library(DataExplorer)
library(caret)
library(VIM)
library(mice)
library(beepr)

test <- read_csv("test.csv")
train <- read_csv("train.csv")

insurance <- bind_rows(train, test, .id = "Set") %>% 
  mutate(Set = if_else(Set == 1, "Train", "Test"))

insurance <- insurance %>% 
  select(-c(Medical_History_10, Medical_History_32, Medical_History_24, Medical_History_15,
            Family_Hist_5, Family_Hist_3, Family_Hist_2, Insurance_History_5))

insurance <- insurance %>% 
  mutate(across(starts_with(c("Medical", "Insur", "Product", "Employment")), .fns = as_factor),
         across(c(Product_Info_4, Ins_Age, Ht, Wt, BMI, Employment_Info_1,
                  Employment_Info_1, Employment_Info_4, Employment_Info_6, 
                  Family_Hist_4, Medical_History_1, Medical_Keyword_24), as.numeric))

insuranceT <- insurance %>% 
  mutate(Medical_History_2 = fct_lump_n(Medical_History_2, 100),
         Product_Info_3 = fct_lump_n(Product_Info_3, 10),
         Employment_Info_2 = fct_lump_n(Employment_Info_2, 10),
         Medical_History_2 = fct_lump_n(Medical_History_2, 20),
         )



insuranceImp <- insurance %>% 
  select(-c(Set, Id, Response)) %>% 
  mice()

insuranceImp %>% write_csv("insuranceImputed")

insurance %>% 
  plot_missing(missing_only = T)
insuranceClean = insuranceImp %>% 
  complete() %>% 
  merge(insurance %>% select(Id, Response))

xyplot(insuranceImp, Employment_Info_6~Employment_Info_1, pch=18, cex=1)


insuranceTest <- insuranceClean %>% 
  filter(Set == "Train")

train_model <- function(alg = 'lm',
                        control = trainControl(method="repeatedcv",
                                               number=5,
                                               repeats=1,
                                               summaryFunction = mape),
                        t.grid = expand.grid(
                          alpha = .1,
                          lambda = .00064
                        ))
{
  model <- train(form=log(Response)~.,
                 data= insuranceTrain),
                 method=alg,
                 trControl=control
                 #,tuneGrid=t.grid
  )
  # preds <- tibble(ACCOUNTNO=props.test$ACCOUNTNO,
  #                           PRED_IMP=exp(predict(model, newdata=props.test %>%
  #                                                   select(-c(IMPROVED, ACCOUNTNO)))))
  # preds <- merge(preds, compare) %>%
  #   mutate(PRED_TOT = PRED_IMP + LAND_VAL) %>%
  #   write_csv("results.csv")
}


model <- train_model(alg = 'glmnet')
# plot(model)
preds <- tibble(ACCOUNTNO=props.test$ACCOUNTNO,
                PRED_IMP=exp(predict(model, newdata=props.test %>%
                                       select(-c(IMPROVED, ACCOUNTNO)))))
results <- merge(preds, compare) %>%
  mutate(PRED_TOT = PRED_IMP + LAND_VAL)
# mod <- lm(LOG_IMPROVED~., data = props.train %>% select(-c(IMPROVED, ACCOUNTNO)))
MAPE(results$PRED_TOT, results$SOLD_PRICE)
