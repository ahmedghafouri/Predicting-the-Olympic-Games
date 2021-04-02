medal_pop_gdp_data_statlearn=read.csv2('C:/Users/Ahmed/Desktop/Statistical Learning/practical assessment 1/medal_pop_gdp_data_statlearn.csv' ,header = TRUE,sep = ",", quote = "\"",dec = ".")
# C:\Users\Ahmed\Desktop\Statistical Learning\practical assessment 1
model2008 = glm(Medal2008 ~Population + GDP ,family = gaussian, data = medal_pop_gdp_data_statlearn)

model2012 = glm(Medal2012 ~Population + GDP ,family = gaussian, data = medal_pop_gdp_data_statlearn)



print(summary(model2008)$coefficients[, 1:3])
print(summary(model2012)$coefficients[, 1:3])


x1=medal_pop_gdp_data_statlearn$GDP
x2=medal_pop_gdp_data_statlearn$Population

model2012 = glm(Medal2012 ~Population + GDP ,family = gaussian, data = medal_pop_gdp_data_statlearn,)
cgdp=summary(model2012)$coefficients[3,1]

cpop=summary(model2012)$coefficients[2,1]

inter=summary(model2012)$coefficients[1,1]
medal2016_pred<-inter+cgdp*x1 + cpop*x2
medal2016_actual<-medal_pop_gdp_data_statlearn$Medal2016




cor(medal2016_pred,medal2016_actual)

plot(medal2016_pred, medal2016_actual)
abline(a=0, b=1)

plot(log(medal2016_pred),log(medal2016_actual))

abline(a=0, b=1)

pop_model <- glm(Medal2012 ~ Population, data = medal_pop_gdp_data_statlearn)
GDP_model <- glm(Medal2012 ~ GDP, data = medal_pop_gdp_data_statlearn)
pop_GDP_model <- glm(Medal2012 ~ GDP+Population, data = medal_pop_gdp_data_statlearn)



X1=medal_pop_gdp_data_statlearn$GDP
X2=medal_pop_gdp_data_statlearn$Population
Y=medal_pop_gdp_data_statlearn$Medal2012
mydata = medal_pop_gdp_data_statlearn

idx = sample(1:71, 50) #sample 100 points in 1...200 without replacement
train_data = medal_pop_gdp_data_statlearn[idx, ]; test_data = medal_pop_gdp_data_statlearn[-idx, ]

formulas = c("Medal2012 ~ Population", "Medal2012 ~ GDP", "Medal2012 ~ GDP+Population")


predictive_log_likelihood = rep(NA, length(formulas))
for (i in 1:length(formulas)){
  
  current_model = glm(formula = formulas[i], data = medal_pop_gdp_data_statlearn)
  
  sigma = sqrt(summary(current_model)$dispersion)
  
  ypredict_mean = predict(current_model, test_data)
  
  predictive_log_likelihood[i] = sum(dnorm(test_data$Medal2012,
                                           ypredict_mean, sigma, log=TRUE))
  
}

plot(1:length(formulas), predictive_log_likelihood,
     xlab="Model Number", ylab="Log Probability")



x1=medal_pop_gdp_data_statlearn$GDP
x2=medal_pop_gdp_data_statlearn$Population

pop_model <- glm(Medal2012 ~ Population, data = medal_pop_gdp_data_statlearn)
GDP_model <- glm(Medal2012 ~ GDP, data = medal_pop_gdp_data_statlearn)
pop_GDP_model <- glm(Medal2012 ~ GDP+Population, data = medal_pop_gdp_data_statlearn)
#################################################

Pop_intercept=summary(pop_model )$coefficients[1,1]
Pop_grad=summary(pop_model )$coefficients[2,1]

pop_pred<-Pop_intercept+Pop_grad*x2
pop_actual<-medal_pop_gdp_data_statlearn$Medal2016
cor(pop_pred,pop_actual)
#############################################
GDP_intercept=summary(GDP_model )$coefficients[1,1]
GDP_grad=summary(GDP_model )$coefficients[2,1]

GDP_pred<-GDP_intercept+GDP_grad*x1
GDP_actual<-medal_pop_gdp_data_statlearn$Medal2016
cor(GDP_pred,GDP_actual)
##########################################

Pop_GDP_intercept=summary(pop_GDP_model)$coefficients[1,1]
Pop_GDP_grad1=summary(pop_GDP_model)$coefficients[2,1]
Pop_GDP_grad2=summary(pop_GDP_model)$coefficients[3,1]

Pop_GDP_pred<-GDP_intercept+Pop_GDP_grad1*x1+Pop_GDP_grad2*x2
Pop_GDP_actual<-medal_pop_gdp_data_statlearn$Medal2016

cor(Pop_GDP_pred,Pop_GDP_actual)
