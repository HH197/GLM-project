library(readr)
car_dat <- read_csv("Downloads/archive (2)/CarPrice_Assignment.csv")
test_string<-toString(car_dat$CarName[1])
strsplit(test_string,"-")
test_string2<-toString(car_dat$CarName[4])
test_vec<-c(strsplit(test_string2," "))


test_car_comp<-c(rep(NA,nrow(car_dat)))
test_car_list<-vector(mode = "list", length = nrow(car_dat))

for(i in 1:nrow(car_dat)){
  test_string<-toString(car_dat$CarName[i])
  test_car_list[[i]]<-c(strsplit(test_string," "))
  test_car_pre<-unlist(test_car_list[[i]][1])
  test_car_comp[i]<-test_car_pre[1]
}


#change the typo
car_comp_final<-c(rep(NA,nrow(car_dat)))

for(i in 1:nrow(car_dat)){
  if(test_car_comp[i]=="maxda"){
    car_comp_final[i]<-"mazda"
  }
  else if(test_car_comp[i]=="Nissan"){
    car_comp_final[i]<-"nissan"
  }
  else if(test_car_comp[i]=="porcshce"){
    car_comp_final[i]<-"toyota"
  }
  else if(test_car_comp[i]=="vokswagen"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="vw"){
    car_comp_final[i]<-"volkswagen"
  }
  else if(test_car_comp[i]=="toyouta"){
    car_comp_final[i]<-"toyota"
  }
  else{
    car_comp_final[i]<-test_car_comp[i]
  }
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

car_comp_final<-firstup(car_comp_final)


car_dat$car_company<-car_comp_final
hist(car_dat$price)
#can use exponential regression or log linear model

#remove the car id and car name because they are not useful in analysis
#car name overlaps with car company variables
car_dat2<-car_dat[,-c(1,3)]

#extract character variables
character_var<-car_dat2[, sapply(car_dat2, class) == 'character']
character_var[sapply(character_var, is.character)] <- lapply(character_var[sapply(character_var, is.character)],
                                                             as.factor)

#extract non-character variables
no_chara<-car_dat2[, sapply(car_dat2, class) != 'character']
#summary(no_chara)
car_dat3<-cbind(character_var,no_chara)

#gamma model with all explanatory variables
gamma_model_price1<-glm(price~., data= car_dat3[, -c(7,9)],family= Gamma(link = "log"))
sum_mod_gamma<-summary(gamma_model_price1)

#using stepAIC to perform stepwise selection
library(MASS)
gamma_step_model <- stepAIC(gamma_model_price1, direction = "both", 
                      trace = TRUE)
gamma_step_sum<-summary(gamma_step_model)


#glm(formula = price ~ aspiration + carbody + enginelocation + 
#cylindernumber + car_company + wheelbase + carlength + carwidth + 
#  carheight + curbweight + enginesize + boreratio + peakrpm + 
#  citympg + highwaympg, family = Gamma(link = "log"), data = car_dat3[,-c(7, 9)])


gamma_square<-glm(formula = price ~ aspiration + carbody + enginelocation + 
                    cylindernumber + car_company + wheelbase + carlength + carwidth + 
                    carheight + curbweight + enginesize + boreratio + peakrpm + 
                    citympg + highwaympg+ I(wheelbase^2)+I(carlength^2)+
                    I(carwidth^2)+I(carheight^2)+I(curbweight^2)+I(enginesize^2)+
                    I(boreratio^2)+I(peakrpm^2)+I(citympg^2)+I(highwaympg^2)
                  , family = Gamma(link = "log"), data = car_dat3[,-c(7, 9)])


gamma_square_step<-stepAIC(gamma_square,direction = "both",trace=FALSE)


#lrttest for full model vs stepwise model (main effect)

loglik_full<-logLik(gamma_model_price1)
loglik_step<-logLik(gamma_step_model)
test_stat1<-loglik_full-loglik_step
p_val1<-1-pchisq(test_stat1,8)
#Our model is the same as full model (main effect)

#lrttest for square(full model) vs stepwise square model
loglik_sq_full<-logLik(gamma_square)
loglik_sq_step<-logLik(gamma_square_step)
test_stat2<-loglik_sq_full-loglik_sq_step
pval2<-1-pchisq(test_stat2,14)

#Our model is the same as full model(square model)

#Comparison of main effect model vs square model
AIC(gamma_step_model)
AIC(gamma_square_step)

#The square model has the lower AIC than main effects model. Hence, the square model is the best. 


#model adequnacy
deviance(gamma_square_step)
pchisq(deviance(gamma_square_step),df.residual(gamma_square_step),lower.tail = FALSE)
#model is adequate


sum_square_step<-summary(gamma_square_step)


#95% t family Confidence Interval for estimates
coefficient_dat<-sum_square_step$coefficients[,c(1,2)]
LL<-coefficient_dat[,1]-qt(0.975,df.residual(gamma_square_step))*coefficient_dat[,2]
UL<-coefficient_dat[,1]+qt(0.975,df.residual(gamma_square_step))*coefficient_dat[,2]
conf_int_dat<-data.frame(coefficient_dat,LL,UL)
names(conf_int_dat)[3]<-"Lower Limit"
names(conf_int_dat)[4]<-"Upper Limit"
(conf_int_dat)



pseudo_R2_square_step<-1-(sum_square_step$deviance/sum_square_step$null.deviance)
#0.9661938


#testing with standardized residual
predict_car<-fitted(gamma_square_step)
price_actual<-car_dat3$price
std_res_car<-(price_actual-predict_car)/predict_car

#standardized residuals constant variance plot 
plot(predict_car,std_res_car,xlab='fitted values',ylab='standardized residuals')
abline(h=0, col="blue")
#analysis: no obvious pattern in the standardized residuals indicating constant 
#variance


#standardized residuals normality plot 
qqnorm(std_res_car,ylab="sample quantiles",xlab="Theoretical Quantiles") 
qqline(std_res_car)

#analysis: all of the standardised residuals lies on the qqline, indicating residuals 
#follow approximately normal distribution
