# ensure the results are repeatable
set.seed(123)
library(caret)
n=caret::createDataPartition(comb.2.2$depression,p=0.5,list=F)
train=comb.2.2[n,]
test=comb.2.2[-n,]

test=test %>% mutate(`Gender: Gender Identity`=fct_relevel(`Gender: Gender Identity`,"Man")) %>% 
  mutate(`Race: What Race Ethnicity`=fct_relevel(`Race: What Race Ethnicity`,"White")) 
train=train %>% mutate(`Gender: Gender Identity`=fct_relevel(`Gender: Gender Identity`,"Man")) %>% 
  mutate(`Race: What Race Ethnicity`=fct_relevel(`Race: What Race Ethnicity`,"White")) 
names(test)=gsub(":","#",names(test))
names(train)=gsub(":","#",names(train))

correlationMatrix <- correlation::correlation(train %>% slice_sample(n=3000),include_factors=T)
correlationMatrix %>% filter(r>0.7) %>% View


mtcars$cyl
model.matrix(am~as.factor(cyl),mtcars)

train %>% names
X <- model.matrix(~age*.+incomecapitaK*.+`Gender: Gender Identity`*.+
                    `Marital Status: Current Marital Status`*.+`The Basics: Sexual Orientation`*.+
                    `Race: What Race Ethnicity`*.+`The Basics: Birthplace`*.
                  ,train %>% select(-depression))
y <- train$depression  # Replace 'response_variable' with your actual response variable


X.all <- model.matrix(~age*.+incomecapitaK*.+`Gender: Gender Identity`*.+
                    `Marital Status: Current Marital Status`*.+`The Basics: Sexual Orientation`*.+
                    `Race: What Race Ethnicity`*.+`The Basics: Birthplace`*.
                  ,comb.2.2 %>% select(-depression))
y.all <- comb.2.2$depression  # Replace 'response_variable' with your actual response variable




set.seed(123)
features=train(x=X,y=y,
               method='glmnet',
               tuneGrid=expand.grid(
                 alpha=seq(1),
                 lambda = seq(0.0001, 0.05, length = 30)  # Range of lambda values to try
               ),
               trControl=trainControl(method='repeatedcv',number=3,repeats=10,verboseIter = T)
)
c=coef(features$finalModel,features$finalModel$tuneValue$lambda) %>% as.matrix

 
  features.lasso.all=train(x=X.all[,which(abs(c)>0)],y=y.all,
                            method='glmnet',family='binomial',
                            tuneGrid=expand.grid(
                              alpha=seq(1),
                              lambda = seq(0, 0.01, length = 30)  # Range of lambda values to try
                            ),
                            trControl=trainControl(method='repeatedcv',number=2,repeats=10,verboseIter = T)
    )
  
  set.seed(123)
  features.lasso.all.2=train(x=X.all,y=y.all,
                           method='glmnet',family='binomial',
                           tuneGrid=expand.grid(
                             alpha=1,
                            # lambda=0.003541379
                             lambda=features$finalModel$tuneValue$lambda
                             #lambda =seq(features$finalModel$tuneValue$lambda/2,features$finalModel$tuneValue$lambda*2,features$finalModel$tuneValue$lambda/2)   # Range of lambda values to try
                           ),
                           trControl=trainControl(method = "none",verboseIter = T) 
                           #trControl=trainControl(method='repeatedcv',number=2,repeats=10,verboseIter = T)
  )
  c.all.2=coef(features.lasso.all.2$finalModel,features.lasso.all.2$finalModel$tuneValue$lambda) %>% as.matrix
  X.all[,c.all.2[which(abs(c.all.2)>0),] %>% names] %>% dim

#### model with all predictors
#  model.base.0=glm(y.all~X.all,family=binomial())
  
### model 2 full regression

model.base=glm(y.all~X.all[,c.all.2[which(abs(c.all.2)>0),] %>% names],family=binomial())


### coef plot
df=data.frame(names=summary(model.base)$coefficients %>% rownames %>%  str_replace(.,fixed('X.all[, c.all.2[which(abs(c.all.2) > 0), ] %>% names]'),''),
           B=summary(model.base)$coefficients[,1],
           p=summary(model.base)$coefficients[,4]
) %>% left_join( 
  data.frame(Bhat=c.all.2[which(abs(c.all.2)>0),],  
  names=c.all.2[which(abs(c.all.2)>0),] %>% names)) %>% 
  slice_sample(n=30)  
  ggplot(data=df,aes(x=names))+geom_line(aes(y=B),group=1,color='darkgreen')+
    geom_point(aes(y=B),color='darkgreen',size=2)+
  geom_point(aes(y=Bhat),color='darkred',size=2)+
    geom_line(aes(y=Bhat),group=1,color='darkred')+
    scale_color_manual(
      values = c("Group A" = "blue", "Group B" = "red"),
      labels = c("Group A" = "Blue Group", "Group B" = "Red Group")
    ) +
    labs(color = "Legend Title")+  # Customize legend title
  theme(legend.position = 'top')+
  scale_x_discrete(labels=1:30)+xlab('Coefficient')+theme_minimal()

### X Y plot
  df.all=data.frame(names=summary(model.base)$coefficients %>% rownames %>%  str_replace(.,fixed('X.all[, c.all.2[which(abs(c.all.2) > 0), ] %>% names]'),''),
                B=summary(model.base)$coefficients[,1],
                p=summary(model.base)$coefficients[,4]
  ) %>% left_join( 
    data.frame(Bhat=c.all.2[which(abs(c.all.2)>0),],  
               names=c.all.2[which(abs(c.all.2)>0),] %>% names)) 
  
  ggplot(data=df.all)+
    geom_point(aes(x=Bhat,y=B),color='darkgreen',size=2)+
    xlim(c(0,2))+
    ylim(c(0,2))+
    geom_line(data=data.frame(x=c(0,2),y=c(0,2)),aes(x=x,y=y),color='black')
    
  geom_point(aes(y=),color='darkred',size=2)+
    geom_line(aes(y=Bhat),group=1,color='darkred')+
    scale_color_manual(
      values = c("Group A" = "blue", "Group B" = "red"),
      labels = c("Group A" = "Blue Group", "Group B" = "Red Group")
    ) +
    labs(color = "Legend Title")+  # Customize legend title
    theme(legend.position = 'top')+
    scale_x_discrete(labels=1:30)+xlab('Coefficient')+theme_minimal()
  
  
  
summary(model.base)$coefficients %>% rownames %>%  str_replace(.,fixed('X.all[, which(abs(c.all.2) > 0)]'),'') %>% length
intersect(c.all.2 %>% rownames) %>% length


c.all.2[which(abs(c.all.2)>0),] %>% length


### GLM
model1=glm(y~X,family=binomial())
### Lasso
model2=glmnet::glmnet(y=y,x=X,family=binomial(),
                      lambda=features$bestTune$lambda
                      #lambda=features.lasso.all.2$finalModel$tuneValue$lambda
                      )
c.model2=coef(model2) %>% as.matrix
c.model2 %>% dim
### GLM on Lasso
model3=glm(y~X[,c.model2[which(abs(c.model2)>0),] %>% names],
                       family='binomial')



### LASSO without area-level

X.2 <- model.matrix(~age*.+incomecapitaK*.+`Gender: Gender Identity`*.+
                    `Marital Status: Current Marital Status`*.+`The Basics: Sexual Orientation`*.+
                    `Race: What Race Ethnicity`*.+`The Basics: Birthplace`*.
                  ,train %>% select(-depression,-matches("Estimate|index|area|county")) )

model.number2=glmnet::glmnet(y=y,x=X.2,family=binomial(),
                      lambda=features$bestTune$lambda
                      #lambda=features.lasso.all.2$finalModel$tuneValue$lambda
)
cc2=coef(model.number2) %>% as.matrix 
cc2[which(abs(cc2)>0),] %>% length

### LASSO without interactions
X.3 <- model.matrix(~.
                    ,train %>% select(-depression) )

model.number3=glmnet::glmnet(y=y,x=X.3,family=binomial(),
                             lambda=features$bestTune$lambda
                             #lambda=features.lasso.all.2$finalModel$tuneValue$lambda
)
cc3=coef(model.number3) %>% as.matrix 
cc3[which(abs(cc3)>0),] %>% length

### tests
X.test <- model.matrix(~age*.+incomecapitaK*.+`Gender: Gender Identity`*.+
                         `Marital Status: Current Marital Status`*.+`The Basics: Sexual Orientation`*.+
                         `Race: What Race Ethnicity`*.+`The Basics: Birthplace`*.
                       ,test %>% select(-depression))
y.test <- test$depression  # Replace 'response_variable' with your actual response variable

X.test.2=X.test 
y.test.2=y.test
#X.test.2=rbind(X.test,X.test[1,]) 
#y.test.2=c(y.test,y.test[1])

f2=factor(ifelse(predict(model1,newdata=as.data.frame(X.test.2))>=0,"Yes","No"))
caret::confusionMatrix(f2,y.test.2)$overall %>% round(3) # glm

f=factor(ifelse(predict(model2,newx=X.test.2)>=0,"Yes","No"))
caret::confusionMatrix(f,y.test.2)$overall %>% round(3) # LASSO

f3=factor(ifelse(predict(model3,newdata=as.data.frame(X.test.2))>=0,"Yes","No"))
caret::confusionMatrix(f3,y.test.2)$overall %>% round(3) # glm on lasso

X.test.model2 <- model.matrix(~age*.+incomecapitaK*.+`Gender: Gender Identity`*.+
                         `Marital Status: Current Marital Status`*.+`The Basics: Sexual Orientation`*.+
                         `Race: What Race Ethnicity`*.+`The Basics: Birthplace`*.
                       ,test %>% select(-depression,-matches("Estimate|index|area|county")))

f4=factor(ifelse(predict(model.number2,newx=X.test.model2)>=0,"Yes","No"))
caret::confusionMatrix(f4,y.test.2)$overall %>% round(3) # LASSO w/o area

X.test.model3 <- model.matrix(~.
                              ,test %>% select(-depression))

f5=factor(ifelse(predict(model.number3,newx=X.test.model3)>=0,"Yes","No"))
caret::confusionMatrix(f5,y.test.2)$overall %>% round(3) # LASSO w/o area



#### plot coefficients on whole dataset
#p=
  model.base %>% summary %>% pluck(coefficients) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  mutate(rowname=str_replace(rowname,fixed('X.all[, c.all.2[which(abs(c.all.2) > 0), ] %>% names]'),'')) %>% 
  #pull(rowname) %>% 
  filter(`Pr(>|z|)`<=0.05) %>% View
  mutate(s1=exp(Estimate)) %>% 
  rename(odds=s1) %>% 
  #filter(str_detect(rowname,"Orientation")) %>% 
  #filter(!str_detect(rowname,'Intercept')) %>% 

  mutate(rowname=gsub('X.2','',rowname)) %>% 
  mutate(rowname=gsub('`(The Basics|Race|Living Situation|Gender|Employment|Home Own|Education Level|Health Insurance)#','',rowname)) %>% 
  mutate(rowname=gsub('`',' ',rowname)) %>% 
  mutate(rowname=gsub('[\\]',' ',rowname)) %>% 
  #mutate(rowname=sub(':-.*?#',':',rowname)) %>%
  #mutate(rowname=gsub('#','_',rowname)) %>% 
  mutate(rowname=gsub(':',' : ',rowname)) %>% 
  mutate(colors=case_when(
    str_detect(rowname,'Estimate\\.\\.Total\\.\\.\\.|religious adherence|area|Area|rate')~"Area-level indicators",
    str_detect(rowname,'Woman')~"Gender: Woman",
    str_detect(rowname,'Identity-Man')~"Gender: Man",
    str_detect(rowname,'Non-heterosexual')~"Sexual oriantation: Non-heterosexual",
    str_detect(rowname,'Hispanic')~"Race/ethnicity: Hispanic",
    str_detect(rowname,'Outside USA')~"Foreign-born",
    str_detect(rowname,'Ethnicity`Other')~"Race/ethnicity: Other",
    str_detect(rowname,'Black')~"Race/ethnicity: Black",
    str_detect(rowname,'White')~"Race/ethnicity: White",
    str_detect(rowname,'USA')~"US-born",
    str_detect(rowname,'Non-binary')~"Gender: Non-binary",
    .default="Other"
  )) %>% #pull(colors) %>% table
  mutate(rowname=gsub('Select all that apply.',' ',rowname)) %>% 
  mutate(rowname=gsub('How much you agree or disagree that ',' ',rowname)) %>%
  mutate(rowname=gsub('Within the past 12 months, were you worried whether ','Worried whether ',rowname)) %>%
  mutate(rowname=gsub(' when you go to a doctor\'s office or other health care provider','',rowname)) %>% 
  mutate(rowname=gsub('Estimate\\.\\.Total\\.\\.\\.','County level rate of ',rowname)) %>%
  mutate(rowname=gsub('\\.',' ',rowname)) %>% 
  dput
  #pull(rowname)
  #mutate(rowname2=gsub('`.*?:','',rowname)) %>% 
  #mutate(rowname2=gsub('`',' ',rowname2)) %>% 
  ggplot(aes(fill=factor(colors)))+geom_col(aes(x=reorder(rowname,odds),y=odds))+
  coord_flip()+
  theme(legend.title='Legend',legend.position='top')+#,color = guide_legend(override.aes = list(shape = NA)))+
  theme_minimal()+
  labs(color = NULL, fill = NULL)+
  #theme(text=element_text(size=20))+
  # Example: Manually setting colors for points or lines
  scale_fill_manual(values = c("Gender: Man"="#8DD3C7", "Sexual oriantation: Non-heterosexual"="#8DD3C7","Area-level indicators"="#FFFFB3", "Race/ethnicity: Black"="#BEBADA", "Gender: Woman"="#FB8072", "Race/ethnicity: Other"="#80B1D3", "Foreign-born"="#FDB462", 
                               "Gender: Non-binary"="#B3DE69", "Race/ethnicity: White"="#FCCDE5", "Other"="#D9D9D9", "Race/ethnicity: Hispanic"="#BC80BD"))+
  geom_point(show.legend = F,aes(x=rowname,y=1),color='darkgrey',size=1)+
  scale_y_continuous(trans = 'log',    breaks = c(0.2, 0.5, 1, 2,5) )+
  theme(legend.position='top')+#,color = guide_legend(override.aes = list(shape = NA)))+
  xlab('')+ylab('Odds ratio') 




